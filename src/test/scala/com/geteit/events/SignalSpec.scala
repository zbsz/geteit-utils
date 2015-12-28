package com.geteit.events

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FeatureSpec, Matchers, RobolectricSuite}

import scala.concurrent.Future
import scala.concurrent.duration._

class SignalSpec extends FeatureSpec with Matchers with RobolectricSuite with ScalaFutures {
  import EventContext.Implicits.global

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

  feature("head") {

    scenario("get head of filtered signal") {
      val source = Signal(1)
      source.filter(_ == 1).head.future.futureValue shouldEqual 1
    }
  }

  def waitForValue(signal: Signal[Int], timeout: FiniteDuration = 100.millis) = {
    @volatile var res = -1
    val sub = signal { res = _ }
    Thread.sleep(timeout.toMillis)
    sub.destroy()
    res
  }
}
