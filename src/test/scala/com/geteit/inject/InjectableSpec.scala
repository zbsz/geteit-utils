package com.geteit.inject

import org.scalatest.{FeatureSpec, Matchers, RobolectricSuite}

class InjectableSpec extends FeatureSpec with Matchers with RobolectricSuite {

  class Storage

  class Service(implicit inj: Injector) extends Injectable {
    val storage = inject[Storage]
  }

  class Service2(implicit inj: Injector) extends Injectable {
    val service = inject[Service]
    val storage = inject[Storage]
  }

  class Controller(implicit inj: Injector) extends Injectable {
    val service = inject[Service]
  }

  val ServiceModule = new Module {
     bind[Storage] to new Storage
     bind[Service] to new Service
  }

  val ServiceModule2 = new Module {
    bind[Storage] to new Storage
    bind[Service2] to new Service2
  }

  val ControllerModule = new Module {
    bind[Controller] to new Controller
  }

  scenario("inject transitive") {
    implicit val injector = ControllerModule :: ServiceModule

    val obj = new Injectable {}
    val controller = obj.inject[Controller]

    controller should not be null
    controller.service should not be null
  }

  scenario("inject local storage in each service module") {
    implicit val injector = ControllerModule :: ServiceModule :: ServiceModule2

    val obj = new Injectable {}
    val serv1 = obj.inject[Service]
    val serv2 = obj.inject[Service2]

    serv1 should not be null
    serv2 should not be null
    (serv1.storage ne serv2.storage) shouldEqual true
  }

  scenario("inject global") {
    implicit val injector = ControllerModule :: ServiceModule :: ServiceModule2

    val obj = new Injectable {}
    val serv2 = obj.inject[Service2]

    serv2 should not be null
    serv2.service should not be null
  }
}
