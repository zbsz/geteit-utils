package com.geteit.events

import org.scalatest.{FeatureSpec, Matchers, RobolectricSuite}

import scala.concurrent.Future
import scala.concurrent.duration._

class SignalSpec extends FeatureSpec with Matchers with RobolectricSuite {

  feature("Threading") {
    import com.geteit.concurrent.Threading.global

    def updateAfterWire = new Signal[Int](Some(0)) {
      override protected def onWire(): Unit = {
        super.onWire()
        Future { set(Some(1)) }
      }
    }

    scenario("synchronize map with initial value") {
      val signal = updateAfterWire map { v =>
        if (v == 0) Thread.sleep(100)
        v
      }
      waitForValue(signal, 200.millis) shouldEqual 1
    }

    scenario("synchronize flatMap with initial value") {
      val signal = updateAfterWire flatMap { v =>
        if (v == 0) Thread.sleep(100)
        Signal.const(v)
      }
      waitForValue(signal, 200.millis) shouldEqual 1
    }
  }

  def waitForValue(signal: Signal[Int], timeout: FiniteDuration = 100.millis) = {
    import EventContext.Implicits.global
    @volatile var res = -1
    val sub = signal { res = _ }
    Thread.sleep(timeout.toMillis)
    sub.destroy()
    res
  }
}
