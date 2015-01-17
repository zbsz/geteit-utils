package com.geteit.concurrent

import java.util.TimerTask

import com.geteit.concurrent.CancellableFuture.CancelException

import scala.concurrent._
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

class CancellableFuture[+A](promise: Promise[A]) extends Awaitable[A] { self =>

  val future = promise.future
  
  def cancel(): Boolean = promise tryFailure CancelException

  def onComplete[B](f: Try[A] => B)(implicit executor: ExecutionContext): Unit = future.onComplete(f)

  def map[B](f: A => B)(implicit executor: ExecutionContext): CancellableFuture[B] = {
    val p = Promise[B]()
    @volatile var cancelFunc = Option(self.cancel _)
    future.onComplete { v =>
      cancelFunc = None
      p tryComplete (v map f) 
    }
    new CancellableFuture(p) {
      override def cancel(): Boolean = {
        if (super.cancel()) {
          Future(cancelFunc.foreach(_.apply()))(CancellableFuture.internalExecutionContext)
          true
        } else false
      }
    }
  }

  def flatMap[B](f: A => CancellableFuture[B])(implicit executor: ExecutionContext): CancellableFuture[B] = {
    val p = Promise[B]()
    @volatile var cancelFunc = Option(self.cancel _)

    self.future onComplete { res =>
      cancelFunc = None
      if (!p.isCompleted) res match {
        case f: Failure[_] => p tryComplete f.asInstanceOf[Failure[B]]
        case Success(v) =>
          try {
            val future = f(v)
            cancelFunc = Some(future.cancel)
            future onComplete { res =>
              cancelFunc = None
              p tryComplete res
            }
            if (p.isCompleted) future.cancel()
          } catch {
            case NonFatal(t) => p failure t
          }
      }
    }

    new CancellableFuture(p) {
      override def cancel(): Boolean = {
        if (super.cancel()) {
          Future(cancelFunc.foreach(_.apply()))(CancellableFuture.internalExecutionContext)
          true
        } else false
      }
    }
  }

  @throws[InterruptedException](classOf[InterruptedException])
  @throws[TimeoutException](classOf[TimeoutException])
  override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    future.ready(atMost)
    this
  }

  @throws[Exception](classOf[Exception])
  override def result(atMost: Duration)(implicit permit: CanAwait): A = future.result(atMost)
}

object CancellableFuture {

  private[concurrent] val internalExecutionContext = LimitedExecutionContext.CpuBoundExecutor

  lazy val DelayTimer = new java.util.Timer("DelayTimer", true)

  object CancelException extends RuntimeException("Operation cancelled")

  class PromiseCompletingRunnable[T](body: => T) extends Runnable {
    val promise = Promise[T]()

    override def run() = {
      if (!promise.isCompleted)
        promise tryComplete {
          try Success(body) catch { case NonFatal(e) => Failure(e) }
        }
    }
  }
  
  def apply[A](body: => A)(implicit executor: ExecutionContext): CancellableFuture[A] = {
    val runnable = new PromiseCompletingRunnable[A](body)
    executor.execute(runnable)
    new CancellableFuture(runnable.promise)
  }

  def lift[A](future: Future[A], onCancel: => Unit = {}): CancellableFuture[A] = {
    val p = Promise[A]()
    p.tryCompleteWith(future)
    new CancellableFuture(p) {
      override def cancel(): Boolean = {
        if (super.cancel()) {
          onCancel
          true
        } else false
      }
    }
  }

  def delay(d: FiniteDuration): CancellableFuture[Unit] = {
    val p = Promise[Unit]()
    val task = new TimerTask {
      override def run(): Unit = p.trySuccess({})
    }
    p.future.onFailure { case CancelException => task.cancel() } (internalExecutionContext)
    DelayTimer.schedule(task, d.toMillis)
    new CancellableFuture(p)
  }

  def delayed[A](d: FiniteDuration, body: => A)(implicit executor: ExecutionContext) = delay(d) map { _ => body }

  def successful[A](result: A): CancellableFuture[A] = new CancellableFuture[A](Promise.successful(result))

  def failed[A](cause: Throwable): CancellableFuture[A] = new CancellableFuture[A](Promise.failed(cause))
}
