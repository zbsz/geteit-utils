package com.geteit.concurrent

import java.util.concurrent.TimeoutException

import com.geteit.concurrent.CancellableFuture.CancelException
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  */
class CancellableFutureSpec extends FeatureSpec with Matchers with BeforeAndAfter {
  
  feature("Await result") {
    
    scenario("Await fast task result") {
      Await.result(CancellableFuture { 1 }, 10.millis) shouldEqual 1
    }

    scenario("Await longer running task result") {
      Await.result(CancellableFuture {
        Thread.sleep(100)
        2
      }, 150.millis) shouldEqual 2
    }
  }

  scenario("Cancel long running task") {
    val future = CancellableFuture { Thread.sleep(100) }
    future.cancel()

    intercept[CancelException.type] {
      Await.result(future, 10.millis) // cancelled task should return immediately
    }
  }

  scenario("Lift and cancel regular Future") {
    val future = CancellableFuture.lift(Future(Thread.sleep(100)))
    future.cancel()

    intercept[CancelException.type] {
      Await.result(future, 10.millis) // cancellable future reports cancellation immediately, even if original feature is still running
    }
  }

  feature("Delayed execution") {

    scenario("Delay future should not return before requested time elapses") {
      intercept[TimeoutException] {
        Await.result(CancellableFuture.delay(200.millis), 150.millis)
      }
    }

    scenario("Wait for delay to complete") {
      Await.result(CancellableFuture.delay(100.millis), 110.millis) shouldEqual {}
    }

    scenario("Cancel delay") {
      val future = CancellableFuture.delay(200.millis)
      future.cancel()

      intercept[CancelException.type] {
        Await.result(future, 10.millis) // cancelled task should return immediately
      }
    }

    scenario("Cancel delayed") {
      val future = CancellableFuture.delayed(200.millis, 1)

      intercept[CancelException.type] {
        future.cancel()
        Await.result(future, 10.millis) // cancelled task should return immediately
      }
    }
  }

  feature("Map") {
    scenario("Await for mapped result") {
      Await.result(CancellableFuture { 1 } map (_ + 1), 10.millis) shouldEqual 2
      Await.result(CancellableFuture { 1 } map (_ + 1) map (_ + 1), 10.millis) shouldEqual 3
      Await.result(CancellableFuture.delayed(100.millis, 1) map (_ + 1), 120.millis) shouldEqual 2
    }

    scenario("Cancel mapped future before map is executed") {
      @volatile var mapped = false
      val future = CancellableFuture.delay(100.millis) .map { _ => mapped = true }
      future.cancel()
      Await.ready(future, 10.millis) // should return immediately
      Thread.sleep(150)
      mapped shouldEqual false
    }

    scenario("Cancel mapped future when map function is being executed") {
      @volatile var mapped = false
      @volatile var completed = false
      val future = CancellableFuture { 1 } .map { _ =>
        mapped = true
        Thread.sleep(100)
        completed = true
        2
      }
      Thread.sleep(10)
      mapped shouldEqual true
      future.cancel()

      intercept[CancelException.type] {
        Await.result(future, 10.millis) // cancellable future reports cancellation immediately, even if some computation is still running
      }
      Thread.sleep(100)
      completed shouldEqual true // map function will complete execution after future was cancelled
    }
  }

  feature("Flatmap") {

    scenario("Await for flatMapped result") {
      Await.result(CancellableFuture(1) flatMap { v => CancellableFuture(v + 1) }, 10.millis) shouldEqual 2
      Await.result(CancellableFuture(1) flatMap { v => CancellableFuture(v + 1) } flatMap { v => CancellableFuture(v + 1) }, 10.millis) shouldEqual 3
    }

    scenario("Execute flatMap recursively") {

      def sum(n: Int, acc: Int = 0): CancellableFuture[Int] = CancellableFuture { n } flatMap {
        case 0 => CancellableFuture.successful(acc)
        case s => sum(s - 1, acc + s)
      }

      Await.result(sum(100), 100.millis) shouldEqual 5050
      Await.result(sum(1000), 1000.millis) shouldEqual 500500
      Await.ready(sum(1000000), 5000.millis)// this will take more time, but should complete pretty fast
    }
    
    scenario("Execute recursive flatMap with high memory usage") {

      def run(n: Int): CancellableFuture[Array[Int]] = CancellableFuture { new Array[Int](32 * 1024 * 1024) } flatMap { arr =>
        if (n == 0) CancellableFuture.successful(arr)
        else run(n - 1)
      }
      
      Await.result(run(100), 5000.millis)
    }
    
    scenario("Cancel flatMapped future before flatMap is executed") {
      @volatile var mapped = false
      val future = CancellableFuture.delay(100.millis) .flatMap { _ =>
        mapped = true
        CancellableFuture.successful(1)
      }
      future.cancel()
      Await.ready(future, 10.millis) // should return immediately
      Thread.sleep(150)
      mapped shouldEqual false
    }

    scenario("Cancel flatMapped future when flatMap function is being executed") {
      @volatile var mapped = false
      @volatile var completed = false
      val future = CancellableFuture { 1 } .flatMap { _ =>
        mapped = true
        Thread.sleep(100)
        completed = true
        CancellableFuture.successful(2)
      }
      Thread.sleep(10)
      mapped shouldEqual true
      future.cancel()

      intercept[CancelException.type] {
        Await.result(future, 10.millis) // cancellable future reports cancellation immediately, even if some computation is still running
      }
      Thread.sleep(100)
      completed shouldEqual true // map function will complete execution after future was cancelled
    }

    scenario("Cancel while flatMapped future is executing") {
      @volatile var mapped = false
      @volatile var completed = false
      val future = CancellableFuture { 1 } .flatMap { _ =>
        mapped = true
        CancellableFuture.delay(100.millis)
      } .flatMap { _ =>
        completed = true
        CancellableFuture.successful(3)
      }
      Thread.sleep(10)
      mapped shouldEqual true
      future.cancel()

      intercept[CancelException.type] {
        Await.result(future, 10.millis) // cancellable future reports cancellation immediately, even if some computation is still running
      }
      Thread.sleep(100)
      completed shouldEqual false // second flatMap should never be executed
    }

    scenario("Cancel while second flatMapped future is executing") {
      @volatile var mapped = false
      @volatile var completed = false
      val future = CancellableFuture { 1 } .flatMap { _ =>
        CancellableFuture { 2 }
      } .flatMap { _ =>
        mapped = true
        CancellableFuture.delay(100.millis)
      } .flatMap { _ =>
        completed = true
        CancellableFuture.successful(3)
      }
      Thread.sleep(10)
      mapped shouldEqual true
      future.cancel()

      intercept[CancelException.type] {
        Await.result(future, 10.millis) // cancellable future reports cancellation immediately, even if some computation is still running
      }
      Thread.sleep(100)
      completed shouldEqual false // second flatMap should never be executed
    }

    scenario("Cancelling base feature should cancel derived ones") {
      @volatile var mapped = false
      @volatile var completed = false
      val future = CancellableFuture.delay(100.millis)
      val future1 = future flatMap { _ =>
        mapped = true
        CancellableFuture.delayed(100.millis, completed = true)
      }
      future.cancel()
      intercept[CancelException.type] {
        Await.result(future1, 10.millis) // cancellable future reports cancellation immediately, even if some computation is still running
      }
      Thread.sleep(110)
      mapped shouldEqual false
    }

    scenario("Cancelling base once completed has no effect on derived ones") {
      @volatile var mapped = false
      @volatile var completed = false
      val future = CancellableFuture.delay(10.millis)
      val future1 = future flatMap { _ =>
        mapped = true
        CancellableFuture.delayed(100.millis, completed = true)
      }
      Thread.sleep(20)
      future.cancel()
      Await.result(future1, 110.millis) // cancellable future reports cancellation immediately, even if some computation is still running
      mapped shouldEqual true
      completed shouldEqual true
    }

    scenario("Cancel while inner most future is executing") {
      @volatile var completed = false

      def sum(n: Int, acc: Int = 0): CancellableFuture[Int] = CancellableFuture { n } flatMap {
        case 0 => CancellableFuture.delayed(2.seconds, { completed = true; acc })
        case s => sum(s - 1, acc + s)
      }

      val future = sum(1000)
      Thread.sleep(100)
      future.cancel()
      intercept[CancelException.type] {
        Await.result(future, 10.millis)
      }

      val future1 = sum(100000)
      Thread.sleep(1000)
      future1.cancel()
      intercept[CancelException.type] {
        Await.result(future1, 10.millis)
      }
      
      Thread.sleep(2000)
      completed shouldEqual false // both futures were cancelled, delayed task should never be executed
    }
  }
}
