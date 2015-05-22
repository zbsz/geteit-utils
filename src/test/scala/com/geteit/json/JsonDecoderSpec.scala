package com.geteit.json

import java.io.StringReader

import com.google.gson.stream.JsonReader
import org.scalatest.{FeatureSpec, Matchers, RobolectricSuite}

class JsonDecoderSpec extends FeatureSpec with Matchers with RobolectricSuite {

  case class Entity(int: Int, float: Float, str: String, opt: Option[String])
  object Entity {
    implicit val Decoder = JsonDecoder[Entity]
  }

  case class Entity1(optArr: Option[Array[Seq[String]]])
  object Entity1 {
    implicit val Decoder = JsonDecoder[Entity1]
  }

  case class EntityContainer(bool: Boolean, dbl: Double, entity: Entity)
  object EntityContainer {
    implicit val Decoder = JsonDecoder[EntityContainer]
  }
  
  case class EntityCollection(containers: Seq[EntityContainer], entities: Array[Entity]) {
    override def equals(obj: scala.Any): Boolean = obj match {
      case EntityCollection(cs, es) => cs == containers && entities.toSeq == es.toSeq
    }
  }
  object EntityCollection {
    implicit val Decoder = JsonDecoder[EntityCollection]
  }
  case class Value(str: String)
  object Value {
    implicit val Decoder = JsonDecoder.valueDecoder[Value]
  }

  case class Obj(v: Value)
  object Obj {
    implicit val Decoder = JsonDecoder[Obj]
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

  scenario("Materialize decoder directly") {
    JsonDecoder[Entity] should not be null
  }

  scenario("Decode json string directly") {
    JsonDecoder[Entity].apply(reader("""{ "int": 1 }""")) shouldEqual Entity(1, 0f, "", None)
  }

  scenario("Access implicit decoder") {
    implicitly[JsonDecoder[Entity]] should not be null
  }

  scenario("Decode json string") {
    implicitly[JsonDecoder[Entity]].apply(reader("""{ "int": 1 }""")) shouldEqual Entity(1, 0f, "", None)
  }

  scenario("Decode value") {
    implicitly[JsonDecoder[Obj]].apply(reader("""{ "v": "str" }""")) shouldEqual Obj(Value("str"))
  }

  scenario("Decode null opt value") {
    implicitly[JsonDecoder[Entity]].apply(reader("""{ "int": 1, "opt": null }""")) shouldEqual Entity(1, 0f, "", None)
  }

  scenario("Decode Entity1") {
    implicitly[JsonDecoder[Entity1]].apply(reader("""{ "int": 1, "opt": null }""")) shouldEqual Entity1(None)
  }

  scenario("Decode container") {
    implicitly[JsonDecoder[EntityContainer]].apply(reader("""{ "bool": true, "entity": { "int": 1, "opt": "test" } }""")) shouldEqual EntityContainer(true, 0.0, Entity(1, 0f, "", Some("test")))
  }

  scenario("Decode collection") {
    implicitly[JsonDecoder[EntityCollection]].apply(reader("""{ "containers": [], "entities": [{ "int": 1, "opt": "test" }] }""")) shouldEqual EntityCollection(Nil, Array(Entity(1, 0f, "", Some("test"))))
  }

  scenario("Skip unknown array and object") {
    implicitly[JsonDecoder[EntityCollection]].apply(reader("""{ "arr": [1, 3, 4], "containers": [], "obj": { "k": "v" }, "entities": [{ "int": 1, "opt": "test" }] }""")) shouldEqual EntityCollection(Nil, Array(Entity(1, 0f, "", Some("test"))))
  }

}
