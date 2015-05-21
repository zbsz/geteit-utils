package com.geteit.json

import com.google.gson.stream.JsonWriter

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait JsonEncoder[T] {
  def apply(v: T, writer: JsonWriter): Unit
}

object JsonEncoder {

  def apply[T]: JsonEncoder[T] = macro impl[T]

  def impl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[JsonEncoder[T]] = {
    import c.universe._

    val sym = c.weakTypeOf[T].typeSymbol
    if (!sym.isClass) c.abort(c.enclosingPosition, s"$sym is not a class")

    val constr = sym.typeSignature.decls.toList.collect { case x: MethodSymbol if x.isPrimaryConstructor => x } .head

    def defaultValue(tpe: c.universe.Type) =
      tpe.typeSymbol.name.toString match {
        case "Long" | "Int" | "Short" | "Byte" | "Float" | "Double" => q"0"
        case "Boolean" => q"false"
        case "String" => Literal(Constant(""))
        case "Option" => q"None"
        case "Seq" => q"Nil"
        case "Array" => q"Array.empty"
        case _ => q"null"
      }

    def writeValue(name: String, tpe: c.universe.Type, obj: TermName, writer: TermName): c.universe.Tree = {
      def write(tpe: c.universe.Type, value: TermName, writer: TermName): c.universe.Tree = {
        tpe.typeSymbol.name.toString match {
          case "Long" | "Int" | "Short" | "Byte" | "Double" | "Float" | "Boolean" | "String" => q"$writer.value($value)"
          case "Option" =>
            val tmp = TermName("$$v")
            val wr = write(tpe.typeArgs.head, tmp, writer)
            q"$value.fold($writer.nullValue()){ ${Ident(tmp)} => $wr}"
          case "Seq" | "Array" | "List" =>
            val tmp = TermName("$param")
            val wr = write(tpe.typeArgs.head, tmp, writer)
            val fn = Function(List(ValDef(Modifiers(Flag.PARAM), tmp, TypeTree(), EmptyTree)), wr)
            val foreach = Apply(Select(Ident(value), TermName("foreach")),List(fn))
            q"""
                $writer.beginArray()
                $foreach
                $writer.endArray()
             """
          case _ => q"implicitly[JsonEncoder[$tpe]].apply($value, $writer)"
        }
      }

      val value = TermName("$v")
      val defVal = defaultValue(tpe)
      val wr = tpe.typeSymbol.name.toString match {
        case "Option" =>
          val tmp = TermName("$$v")
          val wr = write(tpe.typeArgs.head, tmp, writer)
          q"{ val $tmp = $value.get; $wr }"
        case _ => write(tpe, value, writer)
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

    val fields = constr.paramLists.flatten map { s => (s.name.decodedName.toString, s.typeSignature) }

    val writer = TermName("$writer")
    val value = TermName("$value")

    val writes = fields.map { case (name, tpe) => writeValue(name, tpe, value, writer) }

    c.Expr[JsonEncoder[T]](
      q"""
          new JsonEncoder[$sym] {
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
}
