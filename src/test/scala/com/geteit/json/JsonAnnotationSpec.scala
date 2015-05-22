package com.geteit.json

import java.io.{StringWriter, StringReader}

import com.google.gson.stream.{JsonWriter, JsonReader}
import org.scalatest.{FeatureSpec, Matchers, RobolectricSuite}

class JsonAnnotationSpec extends FeatureSpec with Matchers with RobolectricSuite {

  @Json
  case class Entity(int: Int, float: Float, str: String, opt: Option[String])

  @Json
  case class Entity1(optArr: Option[Array[Seq[String]]])
  object Entity1 {
    def apply(str: String): Entity1 = Entity1(Some(Array(Seq(str))))
  }

  def reader(json: String) = new JsonReader(new StringReader(json))

  scenario("Read entity") {
    val r = reader("""{ "int": 1 }""")
    var int = 0
    var float = 0f
    var str = ""

    r.beginObject()
    while (r.hasNext) {
      r.nextName() match {
        case "int" => int = r.nextInt()
        case "float" => float = r.nextDouble().toFloat
        case "str" => str = r.nextString()
      }
    }
  }

  def encode[A: JsonEncoder](value: A) = {
    val out = new StringWriter()
    val writer = new JsonWriter(out)
    implicitly[JsonEncoder[A]].apply(value, writer)
    writer.close()
    out.toString
  }

  scenario("Access implicit ancoder and decoder") {
    implicitly[JsonDecoder[Entity]] should not be null
    implicitly[JsonDecoder[Entity1]] should not be null
    implicitly[JsonEncoder[Entity]] should not be null
    implicitly[JsonEncoder[Entity1]] should not be null
  }

  scenario("Decode json string") {
    implicitly[JsonDecoder[Entity]].apply(reader("""{ "int": 1 }""")) shouldEqual Entity(1, 0f, "", None)
  }

  scenario("Encode entity") {
    encode(Entity(1, 0f, "", None)) shouldEqual """{"int":1}"""
    encode(Entity(2, 1f, "test", Some("str"))) shouldEqual """{"int":2,"float":1.0,"str":"test","opt":"str"}"""
  }
}
