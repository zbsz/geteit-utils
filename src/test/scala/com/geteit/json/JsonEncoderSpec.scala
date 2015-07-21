package com.geteit.json

import java.io.{StringWriter, StringReader}

import com.google.gson.stream.{JsonWriter, JsonReader}
import org.scalatest.{FeatureSpec, Matchers, RobolectricSuite}

class JsonEncoderSpec extends FeatureSpec with Matchers with RobolectricSuite {

  case class Entity(int: Int, float: Float, str: String, opt: Option[String])
  object Entity {
    implicit val Encoder = JsonEncoder[Entity]
  }

  case class Entity1(optArr: Option[Array[Seq[String]]])
  object Entity1 {
    implicit val Encoder = JsonEncoder[Entity1]
  }

  case class EntityContainer(bool: Boolean, dbl: Double, entity: Entity)
  object EntityContainer {
    implicit val Encoder = JsonEncoder[EntityContainer]
  }

  case class EntityCollection(containers: Seq[EntityContainer], entities: Array[Entity]) {
    override def equals(obj: scala.Any): Boolean = obj match {
      case EntityCollection(cs, es) => cs == containers && entities.toSeq == es.toSeq
    }
  }
  object EntityCollection {
    implicit val Encoder = JsonEncoder[EntityCollection]
  }

  case class Value(str: String)
  object Value {
    implicit val Encoder = JsonEncoder.valueEncoder[Value]
  }

  case class Obj(v: Value)
  object Obj {
    implicit val Encoder = JsonEncoder[Obj]
  }

  def encode[A: JsonEncoder](value: A) = {
    val out = new StringWriter()
    val writer = new JsonWriter(out)
    implicitly[JsonEncoder[A]].apply(value, writer)
    writer.close()
    out.toString
  }

  scenario("Materialize encoder directly") {
    JsonEncoder[Entity] should not be null
  }

  scenario("Access implicit encoder") {
    implicitly[JsonEncoder[Entity]] should not be null
  }

  scenario("Encode simple entity") {
    encode(Entity(1, 0f, "", None)) shouldEqual """{"int":1}"""
    encode(Entity(2, 1f, "test", Some("str"))) shouldEqual """{"int":2,"float":1.0,"str":"test","opt":"str"}"""
  }

  scenario("Encode value") {
    encode(Obj(Value("str"))) shouldEqual """{"v":"str"}"""
  }

  scenario("Encode Entity1") {
    encode(Entity1(None)) shouldEqual """{}"""
    encode(Entity1(Some(Array.empty))) shouldEqual """{"optArr":[]}"""
    encode(Entity1(Some(Array(Seq.empty)))) shouldEqual """{"optArr":[[]]}"""
    encode(Entity1(Some(Array(Seq.empty, Seq.empty)))) shouldEqual """{"optArr":[[],[]]}"""
    encode(Entity1(Some(Array(Seq("val", "val1"), Seq.empty)))) shouldEqual """{"optArr":[["val","val1"],[]]}"""
  }

  scenario("Encode container") {
    encode(EntityContainer(false, 0, Entity(0, 0f, "", None))) shouldEqual """{"entity":{}}"""
    encode(EntityContainer(true, 1, Entity(1, 1f, "t", None))) shouldEqual """{"bool":true,"dbl":1.0,"entity":{"int":1,"float":1.0,"str":"t"}}"""
  }

  scenario("Encode collection") {
    encode(EntityCollection(Nil, Array(Entity(1, 0f, "", Some("test"))))) shouldEqual """{"entities":[{"int":1,"opt":"test"}]}"""
  }

  scenario("Encode map") {
    encode(Map[String, Value]("key" -> Value("value"))) shouldEqual """{"key":"value"}"""
  }
}
