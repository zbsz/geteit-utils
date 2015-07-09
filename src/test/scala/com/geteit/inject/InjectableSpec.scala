package com.geteit.inject

import org.scalatest.{FeatureSpec, Matchers, RobolectricSuite}

class InjectableSpec extends FeatureSpec with Matchers with RobolectricSuite {

  class Service(implicit inj: Injector) extends Injectable {}

  class Controller(implicit inj: Injector) extends Injectable {
    val service = inject[Service]
  }

  val ServiceModule = Module { implicit bind =>
     bind[Service] to new Service
  }

  val ControllerModule = Module { implicit bind =>
    bind[Controller] to new Controller
  }

  scenario("inject transitive") {
    implicit val injector = ControllerModule :: ServiceModule

    val obj = new Injectable {}
    val controller = obj.inject[Controller]

    controller should not be null
    controller.service should not be null
  }
}
