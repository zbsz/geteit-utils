package com.geteit.events

import com.geteit.concurrent.Threading
import com.geteit.util.Log._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** A cold signal that loads some initial data whenever the first listener subscribes to it and then - while there are
  * subscribers - listens to updates from a publisher, updating the current data using the received events and the
  * provided update function.
  */
class AggregatingSignal[A, B](source: EventStream[A], load: => Future[B], f: (B, A) => B) extends Signal[B] with EventListener[A] {
  import AggregatingSignal._

  private object valueMonitor
  @volatile private var loadId = 0
  @volatile private var stash = Vector.empty[A]

  override protected[events] def onEvent(event: A, sourceContext: Option[ExecutionContext]): Unit = valueMonitor synchronized {
    value match {
      case Some(v) => AggregatingSignal.this.set(Some(f(v, event)), sourceContext)
      case None    => stash :+= event
    }
  }

  private def startLoading(id: Int) = load.onComplete {
    case Success(s) if id == loadId =>
      valueMonitor.synchronized {
        AggregatingSignal.this.set(Some(stash.foldLeft(s)(f)))
        stash = Vector.empty
      }
    case Failure(ex) if id == loadId =>
      valueMonitor.synchronized(stash = Vector.empty)
      error("load failed", ex)
    case _ =>
      verbose("delegate is no longer the current one, discarding loaded value")
  } (executionContext.getOrElse(Threading.global))

  override def onWire(): Unit = {
    value = None
    stash = Vector.empty
    startLoading(loadId)
    source.subscribe(this)
  }

  override def onUnwire(): Unit = {
    loadId += 1
    source.unsubscribe(this)
  }
}

object AggregatingSignal {
  private implicit val logTag: LogTag = "AggregatingSignal"
}
