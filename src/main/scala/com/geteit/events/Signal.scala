package com.geteit.events

import com.geteit.concurrent.{CancellableFuture, Threading}
import com.geteit.util._
import Log._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import com.geteit.util._

object Signal {
  import language.implicitConversions

  def apply[E](e: E) = new Signal[E](e)
  def apply[E,V](s1: Signal[E], s2: Signal[V]): Signal[(E,V)] = new ZipSignal[E,V](s1, s2)

  def throttled[E](s: Signal[E], delay: FiniteDuration): Signal[E] = new ThrottlingSignal(s, delay)

  def mix[E](sources: Signal[_]*)(f: => E): Signal[E] = new ProxySignal(sources: _*)(f)
  def foldLeft[E,V](sources: Signal[E]*)(v: V)(f: (V, E) => V): Signal[V] = new FoldLeftSignal[E,V](sources: _*)(v)(f)
  def and(sources: Signal[Boolean]*): Signal[Boolean] = new FoldLeftSignal[Boolean, Boolean](sources: _*)(true)(_ && _)
  def or(sources: Signal[Boolean]*): Signal[Boolean] = new FoldLeftSignal[Boolean, Boolean](sources: _*)(true)(_ || _)

  def const[E](value: E): Signal[E] = new ConstantSignal[E](value)

  def future[E](init: E, future: Future[E]): Signal[E] = returning(new Signal(init)) { signal =>
    future.onSuccess { case res => signal.dispatch(res, Some(Threading.global)) } (Threading.global)
  }

  def wrap[E](init: E, source: Publisher[E]): Signal[E] = new Signal[E](init) {
    override protected def onWire(): Unit = source += this
    override protected def onUnwire(): Unit = source -= this
  }

  implicit def signal_to_value[A](s: Signal[A]): A = s.currentValue
}

class Signal[E](@volatile protected[events] var value: E) extends Publisher[E] {
  protected val cachingDisabled = false

  private val lock = new Object
  @volatile protected[events] var wired = false
  @volatile protected[events] var autoWiring = true

  protected lazy val _changed = Publisher[E](executionContext)

  def currentValue: E = {
    if (!wired) {
      warn(s"Accessing value of unwired signal ($this: $value), autowiring will be disabled")(s"Signal[]")
      disableAutowiring()
    }
    value
  }

  def onChanged(s: => Unit)(implicit ec: EventContext): EventObserver[E] = onChanged((_: E) => s)(ec)

  def onChanged(s: Events.Subscriber[E])(implicit ec: EventContext): EventObserver[E] = {
    if (!wired) wire()
    _changed(s)(ec)
  }

  override def subscribe(subscriber: EventObserver[E]): Unit = {
    if (!wired) wire()
    super.subscribe(subscriber)
    if (executionContext.isEmpty) subscriber.apply(value)
    else Future { subscriber.apply(value) } (executionContext.get)
  }

  override def unsubscribe(subscriber: EventObserver[E]): Unit = {
    super.unsubscribe(subscriber)

    if (autoWiring && !hasSubscribers && !_changed.hasSubscribers && wired) {
      unwire()
    }
  }

  def foreach(op: E => Unit)(implicit context: EventContext): Unit = apply(op)(context)

  override def dispatch(event: E, sourceContext: Option[ExecutionContext]): Unit = {
    if (cachingDisabled || this.value != event) {
      this.value = event
      _changed ! this.value
      super.dispatch(event, sourceContext)
    }
  }

  protected def onWire(): Unit = ()
  protected def onUnwire(): Unit = ()

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

    if (autoWiring && !hasSubscribers && !_changed.hasSubscribers && wired) {
      unwire()
    }
  }

  def zip[V](s: Signal[V]): Signal[(E, V)] = new ZipSignal[E, V](this, s)

  def map[V](f: E => V): Signal[V] = new MapSignal[E, V](this, f)
  def filter(f: E => Boolean): Signal[Option[E]] = map(v => Some(v).filter(f))
  def flatMap[V](f: E => Signal[V]): Signal[V] = new FlatMapSignal[E, V](this, f)
  def scan[V](zero: V)(f: (V, E) => V): Signal[V] = new ScanSignal[E, V](this, zero, f)

  def combine[V, T](s: Signal[V])(f: (E, V) => T): Signal[T] = new ProxySignal[T](this, s)(f(value, s.value))

  def throttle(delay: FiniteDuration): Signal[E] = new ThrottlingSignal(this, delay)
}

class ConstantSignal[E](v: E) extends Signal[E](v)

class ThrottlingSignal[E](source: Signal[E], delay: FiniteDuration) extends Signal[E](source.value) {
  import scala.concurrent.duration._
  override protected val cachingDisabled: Boolean = true
  @volatile private var lastDispatched = 0L

  override def onWire(): Unit = {
    source += this
    value = source.value
  }

  override def onUnwire(): Unit = {
    source -= this
  }

  override def dispatch(event: E, sourceContext: Option[ExecutionContext]): Unit = {
    if (this.value != event) {
      this.value = event
      val time = System.currentTimeMillis()
      val context = sourceContext.getOrElse(Threading.global)
      if (lastDispatched < time) {
        val d = math.max(0, lastDispatched + delay.toMillis - time)
        lastDispatched = time + d
        CancellableFuture.delayed(d.millis) { super.dispatch(ThrottlingSignal.this.value, Some(context)) }(context)
      }
    }
  }
}

class FlatMapSignal[E, V](source: Signal[E], f: E => Signal[V]) extends Signal[V](null.asInstanceOf[V]) {
  @volatile private var mapped = f(source.value) // TODO: what about threading, shouldn't f be executed on specific execution context
  value = mapped.value

  private val sourceDelegate = new Publisher[E] {
    protected[events] override def dispatch(v: E, sourceContext: Option[ExecutionContext]): Unit = setupMapped(f(v), sourceContext)
    override def hasSubscribers: Boolean = FlatMapSignal.this.hasSubscribers
    override def unsubscribeAll(): Unit = FlatMapSignal.this.unsubscribeAll()
  }

  override def onWire(): Unit = {
    source += sourceDelegate
    mapped += this

    value = mapped.value
    setupMapped(f(source.value), None)
  }

  override def onUnwire(): Unit = {
    source -= sourceDelegate
    mapped -= this
  }

  private def setupMapped(signal: Signal[V], sourceContext: Option[ExecutionContext]): Unit = {
    if (mapped != signal) {
      mapped -= this
      mapped = signal

      signal += this
      dispatch(signal.value, sourceContext)
    }
  }
}

class ScanSignal[E, V](source: Signal[E], zero: V, f: (V, E) => V) extends Signal[V](zero) {
  private val delegate = new Publisher[E] {
    protected[events] override def dispatch(event: E, sourceContext: Option[ExecutionContext]): Unit = ScanSignal.this.dispatch(f(value, event), sourceContext)
    override def hasSubscribers: Boolean = ScanSignal.this.hasSubscribers
    override def unsubscribeAll(): Unit = ScanSignal.this.unsubscribeAll()
  }

  override def onWire(): Unit = {
    source += delegate
    if (source.value != null) value = f(zero, source.value)
  }

  override def onUnwire(): Unit = source -= delegate
}

class ProxySignal[E](sources: Signal[_]*)(f: => E) extends Signal[E](f) {

  private val delegate = new Publisher[Any] {
    protected[events] override def dispatch(event: Any, sourceContext: Option[ExecutionContext]): Unit = ProxySignal.this.dispatch(f, sourceContext)
    override def hasSubscribers: Boolean = ProxySignal.this.hasSubscribers
    override def unsubscribeAll(): Unit = ProxySignal.this.unsubscribeAll()
  }

  override def onWire(): Unit = {
    sources foreach (_ += delegate)
    value = f
  }

  override def onUnwire(): Unit = {
    sources foreach (_ -= delegate)
  }
}

class MapSignal[E, V](source: Signal[E], f: E => V) extends ProxySignal[V](source)(if (source.value == null) null.asInstanceOf[V] else f(source.value))

class ZipSignal[E, V](s1: Signal[E], s2: Signal[V]) extends ProxySignal[(E, V)](s1, s2)((s1.value, s2.value))

class FoldLeftSignal[E, V](sources: Signal[E]*)(v: V)(f: (V, E) => V) extends ProxySignal[V](sources: _*)(sources.foldLeft(v)((v, signal) => f(v, signal.value)))
