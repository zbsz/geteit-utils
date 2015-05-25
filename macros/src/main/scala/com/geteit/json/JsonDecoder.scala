package com.geteit.json

import java.io.{File, StringReader}
import java.util.Date

import android.net.Uri
import com.google.gson.stream.JsonReader

import scala.concurrent.duration._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait JsonDecoder[T] {
  def apply(reader: JsonReader): T

  def apply(json: String): T = {
    val jr = new JsonReader(new StringReader(json))
    try apply(jr) finally jr.close()
  }
}

object JsonDecoder {

  def apply[T]: JsonDecoder[T] = macro impl[T]

  def valueDecoder[T]: JsonDecoder[T] = macro valueImpl[T]

  def apply[T](f: JsonReader => T) = new JsonDecoder[T] {
    override def apply(reader: JsonReader): T = f(reader)
  }

  implicit val FileDecoder: JsonDecoder[File] = apply(reader => new File(reader.nextString()))
  implicit val DateDecoder: JsonDecoder[Date] = apply(reader => new Date(reader.nextLong()))
  implicit val UriDecoder: JsonDecoder[Uri] = apply(reader => Uri.parse(reader.nextString()))
  implicit val FiniteDurationDecoder: JsonDecoder[FiniteDuration] = apply(_.nextLong().nanos)

  def impl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[JsonDecoder[T]] = {
    import c.universe._

    val sym = c.weakTypeOf[T].typeSymbol
    if (!sym.isClass) c.abort(c.enclosingPosition, s"$sym is not a class")

    val constr = sym.typeSignature.decls.toList.collect { case x: MethodSymbol if x.isPrimaryConstructor => x } .head

    val fields = constr.paramLists.flatten map { s => (s.name.decodedName.toString, s.typeSignature) }

    val vars = fields map { case (name, tpe) => q"var ${TermName(name)}: $tpe = ${JsonEncoder.defaultValue(c)(tpe)}" }

    val reader = TermName("$reader")

    val cases = fields.map { case (name, tpe) => cq"$name => ${TermName(name)} = ${readValue(c)(tpe, reader)}" }

    val defaultCase = cq"_ => $reader.skipValue()"

    val nameMatch = Match(q"$reader.nextName", cases :+ defaultCase)

    val args = fields map { case (name, _) => TermName(name) }

    c.Expr[JsonDecoder[T]](
      q"""
          new com.geteit.json.JsonDecoder[$sym] {
            import com.google.gson.stream._
            override def apply($reader: JsonReader): $sym = {
              ..$vars

              $reader.beginObject()
              while ($reader.hasNext) {
                $nameMatch
              }
              $reader.endObject()

              new $sym(..$args)
            }
          }
      """
    )
  }

  def valueImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[JsonDecoder[T]] = {
    import c.universe._

    val sym = c.weakTypeOf[T].typeSymbol
    if (!sym.isClass) c.abort(c.enclosingPosition, s"$sym is not a class")

    val constr = sym.typeSignature.decls.toList.collect { case x: MethodSymbol if x.isPrimaryConstructor => x } .head
    if (constr.paramLists.flatten.length != 1) c.abort(c.enclosingPosition, s"$sym should have 1 field for json value decoder")

    val tpe = constr.paramLists.head.head.typeSignature
    val reader = TermName("$reader")

    c.Expr[JsonDecoder[T]](
      q"""
          new com.geteit.json.JsonDecoder[$sym] {
            import com.google.gson.stream._
            override def apply($reader: JsonReader): $sym = {
              new $sym(${readValue(c)(tpe, reader)})
            }
          }
      """
    )
  }

  def readValue(c: whitebox.Context)(tpe: c.universe.Type, reader: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    tpe.typeSymbol.name.toString match {
      case "Long" => q"$reader.nextLong"
      case "Int" => q"$reader.nextInt"
      case "Short" => q"$reader.nextInt.toShort"
      case "Byte" => q"$reader.nextInt.toByte"
      case "Double" => q"$reader.nextDouble"
      case "Float" => q"$reader.nextDouble.toFloat"
      case "Boolean" => q"$reader.nextBoolean"
      case "String" => q"$reader.nextString"
      case "Option" =>
        val value = readValue(c)(tpe.typeArgs.head, reader)
        q"if ($reader.peek() == JsonToken.NULL) { $reader.skipValue(); None } else Option($value)"
      case "Seq" | "Array" | "List" => readArray(c)(tpe, reader)
      case _ => q"implicitly[com.geteit.json.JsonDecoder[$tpe]].apply($reader)"
    }
  }

  def typeTree(c: whitebox.Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val name = Ident(TypeName(tpe.typeSymbol.name.toString))
    val params = tpe.typeArgs.map(typeTree(c)(_))
    if (params.isEmpty) name
    else AppliedTypeTree(name, params)
  }

  def readArray(c: whitebox.Context)(tpe: c.universe.Type, reader: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    val builder = TermName("$builder")
    val init = q"val $builder = ${TermName(tpe.typeSymbol.name.toString)}.newBuilder[${typeTree(c)(tpe.typeArgs.head)}]"
    val value = readValue(c)(tpe.typeArgs.head, reader)
    q"""
        $init
       $reader.beginArray()
       while ($reader.hasNext()) { $builder += $value }
       $reader.endArray()
       $builder.result
     """
  }
}
