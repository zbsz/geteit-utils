package com.geteit.util

import java.util.concurrent.ConcurrentLinkedQueue

import com.geteit.concurrent.{CancellableFuture, LimitedExecutionContext}
import com.geteit.util.Log._

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

class SerialProcessingQueue[A](processor: Seq[A] => Future[Any], name: String = "SerialProcessingQueue") {
  protected implicit val dispatcher = new LimitedExecutionContext()
  private implicit val logTag: LogTag = name

  private val queue = new ConcurrentLinkedQueue[A]()
  protected var awaitTask: Future[Any] = Future.successful({})

  def enqueue(event: A): Future[Any] = {
    queue.offer(event)
    processQueue()
  }

  def !(event: A) = enqueue(event)

  def enqueue(events: Seq[A]): Future[Any] = if (events.nonEmpty) {
    events.foreach(queue.offer)
    processQueue()
  } else
    Future.successful(())

  protected def processQueue(): Future[Any] = post(processQueueNow())

  protected def processQueueNow(): Future[Any] = {
    val events = Iterator.continually(queue.poll()).takeWhile(_ != null).toVector
    verbose(s"processQueueNow, events: ${if (events.size > 20) events.size else events}")
    if (events.nonEmpty) processor(events).recoverWithLog()
    else Future.successful(())
  }

  // post some task on this queue, effectively blocking all other processing while this task executes
  def post[T](f: => Future[T]): Future[T] = Future {
    val future = awaitTask.flatMap(_ => f)
    awaitTask = future.recoverWithLog()
    future
  } flatMap identity

  /* just for tests! */
  def clear(): Unit = queue.clear()
}

class ThrottledProcessingQueue[A](delay: FiniteDuration, processor: Seq[A] => Future[Any], name: String = "ThrottledProcessingQueue") extends SerialProcessingQueue[A](processor, name)  {
  @volatile
  private var scheduled = false
  private implicit val logTag: LogTag = name

  override protected def processQueue() = Future {
    if (scheduled) awaitTask
    else {
      verbose(s"processQueue, scheduling next")
      scheduled = true
      post(CancellableFuture.delay(delay).future .flatMap { _ =>
        scheduled = false
        processQueueNow()
      })
    }
  } flatMap identity
}