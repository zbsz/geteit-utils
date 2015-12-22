package com.geteit.json

import java.io.{File, StringReader}
import java.util.Date

import android.net.Uri
import com.google.gson.stream.{JsonToken, JsonReader}

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

  def decode[T: JsonDecoder](json: String): T = implicitly[JsonDecoder[T]].apply(json)

  def decode[T: JsonDecoder](reader: JsonReader): T = implicitly[JsonDecoder[T]].apply(reader)

  def on[A, B: JsonDecoder](f: B => A): JsonDecoder[A] = apply { reader => f(implicitly[JsonDecoder[B]].apply(reader)) }

  private def opt[T](defaultValue: T, read: JsonReader => T) = new JsonDecoder[T] {
    override def apply(reader: JsonReader): T = if (reader.peek() == JsonToken.NULL) { reader.nextNull(); defaultValue } else read(reader)
  }

  implicit val IntDecoder: JsonDecoder[Int] = opt(0, _.nextInt())
  implicit val LongDecoder: JsonDecoder[Long] = opt(0L, _.nextLong())
  implicit val ShortDecoder: JsonDecoder[Short] = opt(0, _.nextInt().toShort)
  implicit val ByteDecoder: JsonDecoder[Byte] = opt(0, _.nextInt().toByte)
  implicit val DoubleDecoder: JsonDecoder[Double] = opt(0d, _.nextDouble())
  implicit val FloatDecoder: JsonDecoder[Float] = opt(0f, _.nextDouble().toFloat)
  implicit val BooleanDecoder: JsonDecoder[Boolean] = opt(false, _.nextBoolean())
  implicit val StringDecoder: JsonDecoder[String] = opt("", _.nextString())
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
        case "Option" => q"if ($reader.peek() == JsonToken.NULL) { $reader.skipValue(); None } else Option(${readValue(tpe.typeArgs.head, reader)})"
        case "Seq" | "Array" | "List" | "Set" | "IndexedSeq" | "ArrayBuffer" | "TreeSet" => readArray(tpe, reader)
        case "Map" => readMap(tpe, reader)
        case _ if !fallbackImplicit && tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass => readCaseClass(tpe, reader)
        case _ => implicitDecoder(tpe, reader)
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
