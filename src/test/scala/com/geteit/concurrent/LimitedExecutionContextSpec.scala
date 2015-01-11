package com.geteit.concurrent

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FeatureSpec, Matchers}

import scala.concurrent.{Await, Future, ExecutionContext}
import concurrent.duration._

/**
 * Tests for LimitedExecutionContext.
 * 
 * Warning: this test should be run on multi core processor machine, will not properly work on single core processor. 
 */
class LimitedExecutionContextSpec extends FeatureSpec with Matchers with GeneratorDrivenPropertyChecks {
  
  feature("Concurrency limiting") {

    scenario("Run multiple tasks on serialized context") {
      implicit val context = new LimitedExecutionContext(1)
      checkMinConcurrentTasks(1)
      checkMaxConcurrentTasks(1)
    }

    scenario("Run multiple tasks on context with limit = 2") {
      implicit val context = new LimitedExecutionContext(2)
      checkMinConcurrentTasks(2)
      checkMaxConcurrentTasks(2)
    }

    scenario("Run multiple tasks on context with limit = cpus count") {
      val cpus = Runtime.getRuntime.availableProcessors()
      implicit val context = new LimitedExecutionContext(cpus)
      checkMinConcurrentTasks(cpus)
      checkMaxConcurrentTasks(cpus)
    }
    
    /**
     * Check that given context is only executing limited number of tasks concurrently.
     */
    def checkMaxConcurrentTasks(maxCount: Int)(implicit context: ExecutionContext) = {
      val taskDelayGenerator = Gen.choose(0L, 10L)
      
      forAll(Gen.listOf(taskDelayGenerator)) { delays =>
        val counter = new ConcurrencyCounter
        delays foreach { delay =>
          context.execute(counter.countedTask(Thread.sleep(delay)))
        }
        counter.awaitComplete(delays.length)
        counter.finishedTasks shouldEqual delays.length
        
        withClue(s"context should execute at most $maxCount tasks concurrently") {
          counter.maxCount should be <= maxCount
        }
      }
    }

    /**
     * Checks that given context is actually able to execute given number of tasks concurrently.
     */
    def checkMinConcurrentTasks(minCount: Int)(implicit context: ExecutionContext) = {
      val counter = new ConcurrencyCounter
      for (_ <- 1 to minCount) {
        context.execute(counter.countedTask(Thread.sleep(10)))
      }
      counter.awaitComplete(minCount)
      counter.finishedTasks shouldEqual minCount

      withClue(s"context should execute $minCount tasks concurrently") {
        counter.maxCount shouldEqual minCount
      }
    }
  }
  
  feature("Serialized execution without races") {
    
    scenario("Cause race with unlimited execution context") {
      val context = new LimitedExecutionContext(8)
      val sum = new RaceCheck(context).check(1000)
      sum should be < 1000
      info(s"sum was $sum instead of 1000 (in case of properly synchronized code)")
    }
    
    scenario("Prevent race by using serialized execution context") {
      val context = new LimitedExecutionContext(1)
      new RaceCheck(context).check(1000) shouldEqual 1000
    }
    
    scenario("Run multiple successful race checks at the same time") {
    
      val checks = Array.fill(16)(new RaceCheck(new LimitedExecutionContext(1, LimitedExecutionContext.UnlimitedExecutor)))
      checks.foreach(_.execute(1000))
      
      Thread.sleep(2000) // wait for all tasks to finish
      
      checks.map(_.sum).toSeq shouldEqual Seq.fill(16)(1000)
    }

    /**
     * Helper executing multiple tasks accessing the same un-synchronized, non-volatile variable.
     * This shared state will be invalid if execution context is not properly synchronized between task executions.
     */
    class RaceCheck(context: ExecutionContext) {
      var sum = 0 // not synchronized, non-volatile var access from concurrent tasks will cause race conditions

      def execute(count: Int) = {
        for (_ <- 1 to count) {
          context.execute(new Runnable() {
            override def run(): Unit = sum += 1
          })
        }
      }

      def check(count: Int) = {
        execute(count)
        Thread.sleep(1000) // wait until tasks finish - 1 second should be enough, but is not guaranteed, we don't want to add any finish counter to not include memory barriers in executed tasks 
        sum
      }
    }
  }
  
  feature("Starvation") {

    /**
     * Runs tasks on two contexts with common serialized parent, 
     * monitors number of tasks executed on each context, 
     * they should be executed at similar pace (differing only in tasks batch size)
     */
    scenario("Run two contexts on the same serialized parent") {
      val parent = new LimitedExecutionContext(1)
      val counter = new ConcurrencyCounter
      val context1 = new LimitedExecutionContext(4, parent)
      val context2 = new LimitedExecutionContext(4, parent)
      
      var balance = 0
      var minBalance = 0
      var maxBalance = 0
      
      def updateBalance(diff: Int) = synchronized {
        balance += diff
        if (balance < minBalance) minBalance = balance
        if (balance > maxBalance) maxBalance = balance
      }
      
      for (_ <- 1 to 1000) {
        context1.execute(counter.countedTask(updateBalance(1)))
        context2.execute(counter.countedTask(updateBalance(-1)))
      }
      counter.awaitComplete(2000)
      counter.maxCount shouldEqual 1
      maxBalance should be <= LimitedExecutionContext.MaxBatchSize + 10
      minBalance should be >= - LimitedExecutionContext.MaxBatchSize - 10
    }
  }

  feature("Using Futures") {
    
    scenario("Dispatch tasks using Future and await termination") {
      implicit val context = new LimitedExecutionContext(1)
      var count = 0 // running on serialized context, so no synchronization needed
      def future = Future(count += 1)
      Await.ready(Future.sequence(Seq.fill(1000)(future)), 1.second)
      count shouldEqual 1000
    }
  }
}

class ConcurrencyCounter {
  
  var currentCount = 0
  var maxCount = 0
  var finishedTasks = 0
  
  def onTaskStart() = synchronized {
    currentCount += 1
    if (currentCount > maxCount) maxCount = currentCount
  }

  def onTaskStop() = synchronized {
    finishedTasks += 1
    currentCount -= 1
    assert(currentCount >= 0)
    
    notifyAll()
  }
  
  def awaitComplete(target: Int) = synchronized {
    while (target > finishedTasks) wait()
  }
  
  def countedTask(task: => Unit) = new Runnable {
    override def run(): Unit = {
      onTaskStart()
      task
      onTaskStop()
    }
  }
}
