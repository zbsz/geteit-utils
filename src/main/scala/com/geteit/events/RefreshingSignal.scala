package com.geteit.events

import com.geteit.concurrent.{LimitedExecutionContext, Threading, CancellableFuture}

import scala.concurrent.{Future, Promise}

class RefreshingSignal[A, B](loader: => CancellableFuture[A], refreshEvent: EventStream[B]) extends Signal[A] {
  private val queue = new LimitedExecutionContext()

  @volatile private var loadFuture = CancellableFuture.cancelled[Unit]()
  @volatile private var subscription = Option.empty[Subscription]

  private def reload() = subscription foreach { _ =>
    loadFuture.cancel()
    val p = Promise[Unit]
    val thisReload = CancellableFuture.lift(p.future)
    loadFuture = thisReload
    loader.onComplete(t => if (loadFuture eq thisReload) p.tryComplete(t.map(v => set(Some(v), Some(Threading.global)))))(queue)
  }

  override protected def onWire(): Unit = {
    super.onWire()
    Future {
      subscription = Some(refreshEvent.on(queue)(_ => reload())(EventContext.Global))
      reload()
    }(queue)
  }

  override protected def onUnwire(): Unit = {
    super.onUnwire()
    Future {
      subscription.foreach(_.unsubscribe())
      subscription = None
      loadFuture.cancel()
      value = None
    }(queue)
  }
}
