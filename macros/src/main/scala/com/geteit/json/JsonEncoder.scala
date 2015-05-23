package com.geteit.json

import java.io.{File, StringWriter}
import java.util.Date

import com.google.gson.stream.JsonWriter

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait JsonEncoder[T] {
  def apply(v: T, writer: JsonWriter): Unit

  def apply(v: T): String = {
    val sw = new StringWriter()
    val jw = new JsonWriter(sw)
    try {
      apply(v, jw)
      jw.close()
      sw.toString
    } finally jw.close()
  }
}

object JsonEncoder {

  def apply[T]: JsonEncoder[T] = macro impl[T]

  def valueEncoder[T]: JsonEncoder[T] = macro valueImpl[T]

  def apply[T](f: (JsonWriter, T) => Unit) = new JsonEncoder[T] {
    override def apply(v: T, writer: JsonWriter): Unit = f(writer, v)
  }

  implicit val FileEncoder: JsonEncoder[File] = apply((w, f) => w.value(f.getAbsolutePath))
  implicit val DateEncoder: JsonEncoder[Date] = apply((w, d) => w.value(d.getTime))


  def impl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[JsonEncoder[T]] = {
    import c.universe._

    def writeValue(name: String, tpe: c.universe.Type, obj: TermName, writer: TermName): c.universe.Tree = {
      val value = TermName("$v")
      val defVal = defaultValue(c)(tpe)
      val wr = tpe.typeSymbol.name.toString match {
        case "Option" =>
          val tmp = TermName("$$v")
          val wr = write(c)(tpe.typeArgs.head, tmp, writer)
          q"{ val $tmp = $value.get; $wr }"
        case _ => write(c)(tpe, value, writer)
      }
      q"""
          {
            val $value = $obj.${TermName(name)}
            if ($value != $defVal) {
              $writer.name($name)
              $wr
            }
          }
       """
    }


    val sym = c.weakTypeOf[T].typeSymbol
    if (!sym.isClass) c.abort(c.enclosingPosition, s"$sym is not a class")

    val constr = sym.typeSignature.decls.toList.collect { case x: MethodSymbol if x.isPrimaryConstructor => x } .head
    val fields = constr.paramLists.flatten map { s => (s.name.decodedName.toString, s.typeSignature) }

    val writer = TermName("$writer")
    val value = TermName("$value")

    val writes = fields.map { case (name, tpe) => writeValue(name, tpe, value, writer) }

    c.Expr[JsonEncoder[T]](
      q"""
          new com.geteit.json.JsonEncoder[$sym] {
            import com.google.gson.stream._
            override def apply($value: $sym, $writer: JsonWriter): Unit = {
              $writer.beginObject()
              ..$writes
              $writer.endObject()
            }
          }
      """
    )
  }

  def valueImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[JsonEncoder[T]] = {
    import c.universe._

    val sym = c.weakTypeOf[T].typeSymbol
    if (!sym.isClass) c.abort(c.enclosingPosition, s"$sym is not a class")

    val constr = sym.typeSignature.decls.toList.collect { case x: MethodSymbol if x.isPrimaryConstructor => x } .head
    if (constr.paramLists.flatten.length != 1) c.abort(c.enclosingPosition, s"$sym should have 1 field for json value encoder")

    val (name, tpe) = constr.paramLists.head.head match { case s => (s.name.decodedName.toString, s.typeSignature) }

    val writer = TermName("$writer")
    val value = TermName("$value")
    val v = TermName("$v")

    c.Expr[JsonEncoder[T]](
      q"""
          new com.geteit.json.JsonEncoder[$sym] {
            import com.google.gson.stream._
            override def apply($value: $sym, $writer: JsonWriter): Unit = {
              val $v = $value.${TermName(name)}
              ${write(c)(tpe, v, writer)}
            }
          }
      """
    )
  }

  def write(c: whitebox.Context)(tpe: c.universe.Type, value: c.universe.TermName, writer: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    tpe.typeSymbol.name.toString match {
      case "Long" | "Int" | "Short" | "Byte" | "Double" | "Float" | "Boolean" | "String" => q"$writer.value($value)"
      case "Option" =>
        val tmp = TermName("$$v")
        val wr = write(c)(tpe.typeArgs.head, tmp, writer)
        q"$value.fold($writer.nullValue()){ ${Ident(tmp)} => $wr}"
      case "Seq" | "Array" | "List" =>
        val tmp = TermName("$param")
        val wr = write(c)(tpe.typeArgs.head, tmp, writer)
        val fn = Function(List(ValDef(Modifiers(Flag.PARAM), tmp, TypeTree(), EmptyTree)), wr)
        val foreach = Apply(Select(Ident(value), TermName("foreach")),List(fn))
        q"""
            $writer.beginArray()
            $foreach
            $writer.endArray()
         """
      case _ => q"implicitly[com.geteit.json.JsonEncoder[$tpe]].apply($value, $writer)"
    }
  }

  def defaultValue(c: whitebox.Context)(tpe: c.universe.Type) = {
    import c.universe._
    tpe.typeSymbol.name.toString match {
      case "Long" | "Int" | "Short" | "Byte" | "Float" | "Double" => q"0"
      case "Boolean" => q"false"
      case "String" => Literal(Constant(""))
      case "Option" => q"None"
      case "Seq" => q"Nil"
      case "Array" => q"Array.empty"
      case _ => q"null"
    }
  }
}
