package com.geteit.json

import com.google.gson.stream.JsonReader

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait JsonDecoder[T] {
  def apply(reader: JsonReader): T
}

object JsonDecoder {

  def apply[T]: JsonDecoder[T] = macro impl[T]

  def impl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[JsonDecoder[T]] = {
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

    def readValue(tpe: c.universe.Type, reader: TermName): c.universe.Tree = {
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
          val value = readValue(tpe.typeArgs.head, reader)
          q"if ($reader.peek() == JsonToken.NULL) { $reader.skipValue(); None } else Option($value)"
        case "Seq" | "Array" | "List" => readArray(tpe, reader)
        case _ => q"implicitly[JsonDecoder[$tpe]].apply($reader)"
      }
    }

    def typeTree(tpe: c.universe.Type): c.universe.Tree = {
      val name = Ident(TypeName(tpe.typeSymbol.name.toString))
      val params = tpe.typeArgs.map(typeTree(_))
      if (params.isEmpty) name
      else AppliedTypeTree(name, params)
    }

    def readArray(tpe: c.universe.Type, reader: TermName): c.universe.Tree = {
      val builder = TermName("$builder")
      val init = q"val $builder = ${TermName(tpe.typeSymbol.name.toString)}.newBuilder[${typeTree(tpe.typeArgs.head)}]"
      val value = readValue(tpe.typeArgs.head, reader)
      q"""
          $init
         $reader.beginArray()
         while ($reader.hasNext()) { $builder += $value }
         $reader.endArray()
         $builder.result
       """
    }

    val fields = constr.paramLists.flatten map { s => (s.name.decodedName.toString, s.typeSignature) }

    val vars = fields map { case (name, tpe) => q"var ${TermName(name)}: $tpe = ${defaultValue(tpe)}" }

    val reader = TermName("$reader")

    val cases = fields.map { case (name, tpe) => cq"$name => ${TermName(name)} = ${readValue(tpe, reader)}" }

    val defaultCase = cq"_ => $reader.skipValue()"

    val nameMatch = Match(q"$reader.nextName", cases :+ defaultCase)

    val args = fields map { case (name, _) => TermName(name) }

    c.Expr[JsonDecoder[T]](
      q"""
          new JsonDecoder[$sym] {
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
}
