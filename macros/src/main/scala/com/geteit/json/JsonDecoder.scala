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

  def decode[T: JsonDecoder](json: String) = implicitly[JsonDecoder[T]].apply(json)

  implicit val FileDecoder: JsonDecoder[File] = apply(reader => new File(reader.nextString()))
  implicit val DateDecoder: JsonDecoder[Date] = apply(reader => new Date(reader.nextLong()))
  implicit val UriDecoder: JsonDecoder[Uri] = apply(reader => Uri.parse(reader.nextString()))
  implicit val FiniteDurationDecoder: JsonDecoder[FiniteDuration] = apply(_.nextLong().nanos)

  def impl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[JsonDecoder[T]] = ImplHelper(c)

  def valueImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Expr[JsonDecoder[T]] = ImplHelper(c, value = true)

  def ImplHelper[T: c.WeakTypeTag](c: whitebox.Context, value: Boolean = false): c.Expr[JsonDecoder[T]] = {
    import c.universe._

    def readCaseClass(tpe: Type, reader: TermName): Tree = {
      val sym = tpe.typeSymbol

      val constr = sym.typeSignature.decls.toList.collect { case x: MethodSymbol if x.isPrimaryConstructor => x } .head
      val fields = constr.paramLists.flatten map { s => (s.name.decodedName.toString, s.typeSignature) }
      val vars = fields map { case (name, tpe) => q"var ${TermName(name)}: $tpe = ${JsonEncoder.defaultValue(c)(tpe)}" }
      val cases = fields.map { case (name, tpe) => cq"$name => ${TermName(name)} = ${readValue(tpe, reader)}" }
      val defaultCase = cq"_ => $reader.skipValue()"
      val nameMatch = Match(q"$reader.nextName", cases :+ defaultCase)
      val args = fields map { case (name, _) => TermName(name) }

      q"""
        {
          ..$vars

          $reader.beginObject()
          while ($reader.hasNext) {
            $nameMatch
          }
          $reader.endObject()

          new $sym(..$args)
        }
      """
    }

    def implicitDecoder(tpe: Type, reader: TermName): Tree =
      q"implicitly[com.geteit.json.JsonDecoder[$tpe]].apply($reader)"

    def readValue(tpe: Type, reader: TermName, fallbackImplicit: Boolean = true): Tree = {
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
        case "Seq" | "Array" | "List" | "Set" => readArray(tpe, reader)
        case "Map" => readMap(tpe, reader)
        case _ => if (fallbackImplicit) implicitDecoder(tpe, reader) else readCaseClass(tpe, reader)
      }
    }

    def readMap(tpe: Type, reader: TermName): Tree = {
      val valueTpe = tpe.typeArgs(1)
      val builder = TermName("$builder")

      q"""
        val $builder = Map.newBuilder[String, ${typeTree(valueTpe)}]
        $reader.beginObject()
        while ($reader.hasNext) {
           $builder += $reader.nextName() -> ${readValue(valueTpe, reader)}
        }
        $reader.endObject()
        $builder.result()
      """
    }

    def typeTree(tpe: Type): Tree = {
      val name = Ident(TypeName(tpe.typeSymbol.name.toString))
      val params = tpe.typeArgs.map(typeTree(_))
      if (params.isEmpty) name
      else AppliedTypeTree(name, params)
    }

    def readArray(tpe: Type, reader: TermName): Tree = {
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

    val reader = TermName("$reader")

    if (value) {
      val sym = c.weakTypeOf[T].typeSymbol
      val constr = sym.typeSignature.decls.toList.collect { case x: MethodSymbol if x.isPrimaryConstructor => x } .head
      if (constr.paramLists.flatten.length != 1) c.abort(c.enclosingPosition, s"$sym should have 1 field for json value decoder")
      val tpe = constr.paramLists.head.head.typeSignature

      c.Expr[JsonDecoder[T]](
        q"""
            new com.geteit.json.JsonDecoder[$sym] {
              import com.google.gson.stream._
              override def apply($reader: JsonReader): $sym = {
                new $sym(${readValue(tpe, reader)})
              }
            }
        """
      )
    } else {
      val aType = c.weakTypeOf[T]
      val read = readValue(aType, reader, fallbackImplicit = false)

      c.Expr[JsonDecoder[T]](
        q"""
            new com.geteit.json.JsonDecoder[$aType] {
              import com.google.gson.stream._
              override def apply($reader: JsonReader): $aType = $read
            }
        """
      )
    }
  }
}
