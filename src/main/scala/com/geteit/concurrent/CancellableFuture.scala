package com.geteit.concurrent

import java.util.TimerTask

import com.geteit.concurrent.CancellableFuture.CancelException
import com.geteit.util.LoggedTry

import scala.concurrent._
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.control.{NoStackTrace, NonFatal}
import scala.util.{Failure, Success, Try}

class CancellableFuture[+A](promise: Promise[A]) extends Awaitable[A] { self =>

  val future = promise.future
  
  def cancel(): Boolean = promise tryFailure CancelException

  def onComplete[B](f: Try[A] => B)(implicit executor: ExecutionContext): Unit = future.onComplete(f)

  def onSuccess[U](pf: PartialFunction[A, U])(implicit executor: ExecutionContext): Unit = future.onSuccess(pf)

  def onFailure[U](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): Unit = future.onFailure(pf)

  def onCancelled(body: => Unit)(implicit executor: ExecutionContext): Unit = future.onFailure {
    case CancelException => body
  }

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

  def recover[B >: A](pf: PartialFunction[Throwable, B])(implicit executor: ExecutionContext) = recoverWith(pf.andThen(CancellableFuture.successful))

  def recoverWith[U >: A](pf: PartialFunction[Throwable, CancellableFuture[U]])(implicit executor: ExecutionContext): CancellableFuture[U] = {
    val p = Promise[U]()
    @volatile var cancelFunc = Option(self.cancel _)

    future.onComplete { res =>
      cancelFunc = None
      if (!p.isCompleted) res match {
        case Failure(t) if pf.isDefinedAt(t) =>
          val fut = pf.applyOrElse(t, (_: Throwable) => this)
          cancelFunc = Some(fut.cancel)
          fut onComplete { res =>
            cancelFunc = None
            p tryComplete res
          }
          if (p.isCompleted) fut.cancel()
        case other =>
          p tryComplete other
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

  def flatten[B](implicit executor: ExecutionContext, evidence: A <:< CancellableFuture[B]): CancellableFuture[B] = flatMap(x => x)

  def zip[B](other: CancellableFuture[B])(implicit executor: ExecutionContext): CancellableFuture[(A, B)] = CancellableFuture.zip(self, other)

  @throws[InterruptedException](classOf[InterruptedException])
  @throws[TimeoutException](classOf[TimeoutException])
  override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
    future.ready(atMost)
    this
  }

  @throws[Exception](classOf[Exception])
  override def result(atMost: Duration)(implicit permit: CanAwait): A = future.result(atMost)

  def withTimeout(timeout: FiniteDuration): CancellableFuture[A] = {
    implicit val ec = CancellableFuture.internalExecutionContext
    val f = CancellableFuture.delayed(timeout)(this.promise.tryFailure(new TimeoutException(s"withTimeout($timeout) elapsed")))
    onComplete(_ => f.cancel())
    this
  }
}

object CancellableFuture {

  private[concurrent] val internalExecutionContext = LimitedExecutionContext.CpuBoundExecutor

  lazy val DelayTimer = new java.util.Timer("DelayTimer", true)

  case object CancelException extends RuntimeException("Operation cancelled") with NoStackTrace

  class PromiseCompletingRunnable[T](body: => T) extends Runnable {
    val promise = Promise[T]()

    override def run() = {
      if (!promise.isCompleted)
        promise tryComplete LoggedTry(body)("CancellableFuture")
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
    if (d <= Duration.Zero) successful(())
    else {
      val p = Promise[Unit]()
      val task = new TimerTask {
        override def run(): Unit = p.trySuccess({})
      }
      p.future.onFailure { case CancelException => task.cancel() } (internalExecutionContext)
      DelayTimer.schedule(task, d.toMillis)
      new CancellableFuture(p)
    }
  }

  def delayed[A](d: FiniteDuration)(body: => A)(implicit executor: ExecutionContext) = delay(d) map { _ => body }

  def successful[A](result: A): CancellableFuture[A] = new CancellableFuture[A](Promise.successful(result))

  def failed[A](cause: Throwable): CancellableFuture[A] = new CancellableFuture[A](Promise.failed(cause))

  def cancelled[A](): CancellableFuture[A] = failed(CancelException)

  def sequence[A](in: Seq[CancellableFuture[A]])(implicit executor: ExecutionContext): CancellableFuture[Seq[A]] =
    in.foldLeft(successful(Seq.newBuilder[A])) {
      (fr, fa) => for (r <- fr; a <- fa) yield r += a
    } map (_.result())

  def traverse[A, B](in: Seq[A])(f: A => CancellableFuture[B])(implicit executor: ExecutionContext): CancellableFuture[Seq[B]] = sequence(in.map(f))

  def traverseSequential[A, B](in: Seq[A])(f: A => CancellableFuture[B])(implicit executor: ExecutionContext): CancellableFuture[Seq[B]] = {
    def processNext(remaining: Seq[A], acc: List[B] = Nil): CancellableFuture[Seq[B]] =
      if (remaining.isEmpty) CancellableFuture.successful(acc.reverse)
      else f(remaining.head) flatMap { res => processNext(remaining.tail, res :: acc) }

    processNext(in)
  }

  def zip[A, B](f1: CancellableFuture[A], f2: CancellableFuture[B])(implicit executor: ExecutionContext): CancellableFuture[(A, B)] = {
    val p = Promise[(A, B)]()

    p.tryCompleteWith((for (r1 <- f1; r2 <- f2) yield (r1, r2)).future)

    new CancellableFuture(p) {
      override def cancel(): Boolean = {
        if (super.cancel()) {
          Future {
            f1.cancel()
            f2.cancel()
          }(CancellableFuture.internalExecutionContext)
          true
        } else false
      }
    }
  }
}
