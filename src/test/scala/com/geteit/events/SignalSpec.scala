package com.geteit.events

import com.geteit.concurrent.Threading
import org.robolectric.shadows.ShadowLooper
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

  feature("Subscription execution context") {

    scenario("Don't trampoline when no context is required") {
      val signal = Signal[Int]()

      var res = 0
      signal { v => Thread.sleep(100); res = v }
      signal ! 1

      res shouldEqual 1
    }

    scenario("Don't trampoline when contexts match") {
      val signal = Signal[Int]()

      var res = 0
      signal.on(Threading.global) { v => Thread.sleep(100); res = v }
      signal.publish(2, Threading.global)

      res shouldEqual 2
    }

    scenario("Trampoline when source context is not specified") {
      val signal = Signal[Int]()

      @volatile var res = 0
      signal.on(Threading.global) { v => Thread.sleep(100); res = v }
      signal ! 3

      res shouldEqual 0

      Thread.sleep(250)
      res shouldEqual 3
    }

    scenario("Trampoline when contexts don't match") {
      val signal = Signal[Int]()

      @volatile var res = 0
      signal.on(Threading.global) { v => Thread.sleep(100); res = v }
      signal.publish(4, Threading.ui)

      res shouldEqual 0

      Thread.sleep(250)
      res shouldEqual 4
    }

    scenario("Don't trampoline composed signal when contexts match") {
      val source = Signal[Int]()
      val source1 = Signal[Signal[Int]]
      val signal = source1.flatMap(identity).map(_ + 1)

      source1 ! source

      var res = 0
      signal.on(Threading.global) { v => Thread.sleep(100); res = v }
      source.publish(5, Threading.global)

      res shouldEqual 6
    }
  }

  feature("head") {

    scenario("get head of filtered signal") {
      val source = Signal(10)
      source.filter(_ == 10).head.future.futureValue shouldEqual 10
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
