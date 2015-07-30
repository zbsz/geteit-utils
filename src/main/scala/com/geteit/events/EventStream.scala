package com.geteit.events

import com.geteit.concurrent.{Threading, CancellableFuture}
import com.geteit.events.Events.Subscriber
import com.geteit.util._

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.ref.WeakReference

private[events] trait EventListener[E] {
  protected[events] def onEvent(event: E, ec: Option[ExecutionContext]): Unit
}

object EventStream {
  def apply[A]() = new SourceStream[A]
  def union[A](streams: EventStream[A]*): EventStream[A] = new UnionEventStream(streams: _*)

  def wrap[A](source: Signal[A]): EventStream[A] = new EventStream[A] with SignalListener {
    override def changed(ec: Option[ExecutionContext]): Unit = source.value.foreach(v => dispatch(v, ec))
    override protected def onWire(): Unit = source.subscribe(this)
    override protected def onUnwire(): Unit = source.unsubscribe(this)
  }
}

class SourceStream[E] extends EventStream[E] {
  def !(event: E): Unit = publish(event)
  override def publish(event: E): Unit = dispatch(event, None)
}

class EventStream[E] extends EventSource[E] with Observable[EventListener[E]] {

  private object dispatchMonitor

  private def dispatchEvent(event: E, currentExecutionContext: Option[ExecutionContext]): Unit = dispatchMonitor.synchronized {
    notifyListeners(_.onEvent(event, currentExecutionContext))
  }

  protected[events] def dispatch(event: E, sourceContext: Option[ExecutionContext]): Unit = executionContext match {
    case None | `sourceContext` => dispatchEvent(event, sourceContext)
    case Some(ctx) => Future(dispatchEvent(event, executionContext))(ctx)
  }

  protected def publish(event: E) = dispatch(event, None)

  override def on(ec: ExecutionContext)(subscriber: Subscriber[E])(implicit eventContext: EventContext): Subscription = returning(new StreamSubscription[E](this, subscriber, Some(ec))(WeakReference(eventContext)))(_.enable())

  override def apply(subscriber: Subscriber[E])(implicit eventContext: EventContext): Subscription = returning(new StreamSubscription[E](this, subscriber, None)(WeakReference(eventContext)))(_.enable())

  def foreach(op: E => Unit)(implicit context: EventContext): Unit = apply(op)(context)

  def map[V](f: E => V): EventStream[V] = new MapEventStream[E, V](this, f)
  def filter(f: E => Boolean): EventStream[E] = new FilterEventStream[E](this, f)
  def scan[V](zero: V)(f: (V, E) => V): EventStream[V] = new ScanEventStream[E, V](this, zero, f)

  def next(implicit context: EventContext): CancellableFuture[E] = {
    val p = Promise[E]()
    val o = apply { p.trySuccess(_) }
    p.future.onComplete(_ => o.destroy())(Threading.global)
    new CancellableFuture(p)
  }

  protected def onWire(): Unit = {}
  protected def onUnwire(): Unit = {}
}

abstract class ProxyEventStream[A, E](sources: EventStream[A]*) extends EventStream[E] with EventListener[A] {
  override protected def onWire() = sources foreach (_.subscribe(this))
  override protected def onUnwire() = sources foreach (_.unsubscribe(this))
}

class MapEventStream[E, V](source: EventStream[E], f: E => V) extends ProxyEventStream[E, V](source) {
  override protected[events] def onEvent(event: E, sourceContext: Option[ExecutionContext]): Unit = dispatch(f(event), sourceContext)
}

class FilterEventStream[E](source: EventStream[E], f: E => Boolean) extends ProxyEventStream[E, E](source) {
  override protected[events] def onEvent(event: E, sourceContext: Option[ExecutionContext]): Unit = if (f(event)) dispatch(event, sourceContext)
}

class UnionEventStream[E](sources: EventStream[E]*) extends ProxyEventStream[E, E](sources: _*) {
  override protected[events] def onEvent(event: E, sourceContext: Option[ExecutionContext]): Unit = dispatch(event, sourceContext)
}

class ScanEventStream[E, V](source: EventStream[E], zero: V, f: (V, E) => V) extends ProxyEventStream[E, V] {
  @volatile private var value = zero

  override protected[events] def onEvent(event: E, sourceContext: Option[ExecutionContext]): Unit = {
    value = f(value, event)
    dispatch(value, sourceContext)
  }
}
