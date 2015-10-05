package com.geteit.concurrent

import java.util.concurrent.{Executors, ConcurrentLinkedQueue}
import java.util.concurrent.atomic.AtomicInteger

import com.geteit.util.Log._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

/**
 * Execution context limiting number of concurrently executing tasks.
 * All tasks are executed on parent execution context.
 */
class LimitedExecutionContext(concurrencyLimit: Int = 1, parent: ExecutionContext = LimitedExecutionContext.CpuBoundExecutor) extends ExecutionContext {
  import LimitedExecutionContext._
  require(concurrencyLimit >= 1)

  override def execute(runnable: Runnable): Unit = Executor.dispatch(runnable)

  override def reportFailure(cause: Throwable): Unit = {
    error("reportFailure", cause)
    parent.reportFailure(cause)
  }

  private object Executor extends Runnable {

    val queue = new ConcurrentLinkedQueue[Runnable]
    val runningCount = new AtomicInteger(0)

    def dispatch(runnable: Runnable): Unit = {
      queue.add(runnable)
      dispatchExecutor()
    }

    def dispatchExecutor(): Unit = {
      if (runningCount.getAndIncrement < concurrencyLimit) 
        parent.execute(this)
      else if (runningCount.decrementAndGet() < concurrencyLimit && !queue.isEmpty) 
        dispatchExecutor() // to prevent race condition when executor has just finished
    }

    override def run(): Unit = {
      
      @tailrec
      def executeBatch(counter: Int = 0): Unit = queue.poll() match {
        case null => // done
        case runnable =>
          try {
            runnable.run()
          } catch {
            case cause: Throwable => reportFailure(cause)
          }
          if (counter < LimitedExecutionContext.MaxBatchSize) executeBatch(counter + 1)
      }

      executeBatch()

      if (runningCount.decrementAndGet() < concurrencyLimit && !queue.isEmpty) 
        dispatchExecutor()
    }
  }
}

object LimitedExecutionContext {
  private implicit val tag: LogTag = "LimitedExecutionContext"
  /**
   * Maximum number of tasks to execute in single batch.
   * Used to prevent starving of other contexts using common parent.
   */
  val MaxBatchSize = 100

  val UnlimitedExecutor = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  val CpuBoundExecutor = new LimitedExecutionContext(Runtime.getRuntime.availableProcessors() max 2, UnlimitedExecutor)
}
