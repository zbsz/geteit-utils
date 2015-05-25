package com.geteit.events

import com.geteit.concurrent.CancellableFuture

import scala.concurrent.{ExecutionContext, Promise}

object EventStream {
  def union[A](streams: EventStream[A]*): EventStream[A] = new UnionEventStream(streams: _*)

  def apply[E](publisher: Publisher[E]): EventStream[E] = new ProxyEventStream[E, E](publisher) {
    override protected def onEvent(event: E, sourceContext: Option[ExecutionContext]): Unit = dispatch(event, sourceContext)
  }
}

class EventStream[E] extends Publisher[E] {

  def foreach(op: E => Unit)(implicit context: EventContext): Unit = apply(op)(context)

  def map[V](f: E => V): EventStream[V] = new MapEventStream[E, V](this, f)
  def filter(f: E => Boolean): EventStream[E] = new FilterEventStream[E](this, f)
  def scan[V](zero: V)(f: (V, E) => V): EventStream[V] = new ScanEventStream[E, V](this, zero, f)

  def next(implicit context: EventContext): CancellableFuture[E] = {
    val p = Promise[E]()
    val o = apply { p.trySuccess(_) }
    p.future.onComplete(_ => o.destroy())(com.geteit.concurrent.Threading.global)
    new CancellableFuture(p)
  }

  private val lock = new Object
  @volatile protected[events] var wired = false
  @volatile protected[events] var autoWiring = true
  
  protected def onWire(): Unit = {}
  protected def onUnwire(): Unit = {}

  protected def wire(): Unit = lock.synchronized {
    if (!wired) {
      wired = true // XXX: this has to be before onWire to prevent stack overflow (onWire -> currentValue -> disableAutowiring -> wire -> onWire)
      onWire()
    }
  }

  protected def unwire(): Unit = lock.synchronized {
    if (wired) {
      onUnwire()
      wired = false
    }
  }

  def disableAutowiring(): Unit = {
    autoWiring = false
    if (!wired) wire()
  }


  override private[events] def +=(child: Publisher[_ >: E]): Unit = {
    if (!wired) wire()
    super.+=(child)
  }

  override private[events] def -=(child: Publisher[_ >: E]): Unit = {
    super.-=(child)

    if (autoWiring && !hasSubscribers && wired) {
      unwire()
    }
  }

  override def subscribe(subscriber: EventObserver[E]): Unit = {
    if (!wired) wire()
    super.subscribe(subscriber)
  }

  override def unsubscribe(subscriber: EventObserver[E]): Unit = {
    super.unsubscribe(subscriber)

    if (autoWiring && !hasSubscribers && wired) {
      unwire()
    }
  }
}

abstract class ProxyEventStream[A, E](sources: Publisher[A]*) extends EventStream[E] {

  private val delegate = new Publisher[A] {
    protected[events] override def dispatch(event: A, sourceContext: Option[ExecutionContext]): Unit = ProxyEventStream.this.onEvent(event, sourceContext)
    override def hasSubscribers: Boolean = ProxyEventStream.this.hasSubscribers
    override def unsubscribeAll(): Unit = ProxyEventStream.this.unsubscribeAll()
  }

  protected def onEvent(event: A, sourceContext: Option[ExecutionContext]): Unit

  override protected def onWire() = {
    sources foreach (_ += delegate)
  }

  override protected def onUnwire() = {
    sources foreach (_ -= delegate)
  }
}

class MapEventStream[E, V](source: EventStream[E], f: E => V) extends ProxyEventStream[E, V](source) {
  override protected def onEvent(event: E, sourceContext: Option[ExecutionContext]): Unit = dispatch(f(event), sourceContext)
}

class FilterEventStream[E](source: EventStream[E], f: E => Boolean) extends ProxyEventStream[E, E](source) {
  override protected def onEvent(event: E, sourceContext: Option[ExecutionContext]): Unit = if (f(event)) dispatch(event, sourceContext)
}

class UnionEventStream[E](sources: EventStream[E]*) extends ProxyEventStream[E, E](sources: _*) {
  override protected def onEvent(event: E, sourceContext: Option[ExecutionContext]): Unit = dispatch(event, sourceContext)
}

class ScanEventStream[E, V](source: EventStream[E], zero: V, f: (V, E) => V) extends ProxyEventStream[E, V] {
  @volatile private var value = zero

  override protected def onEvent(event: E, sourceContext: Option[ExecutionContext]): Unit = {
    value = f(value, event)
    dispatch(value, sourceContext)
  }
}
