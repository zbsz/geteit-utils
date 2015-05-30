package com.geteit.events

import com.geteit.concurrent.{CancellableFuture, Threading}
import com.geteit.util._
import Log._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import com.geteit.util._

object Signal {
  import language.implicitConversions

  def apply[E](e: E) = new Signal[E] {
    value = Some(e)
  }
  def apply[E,V](s1: Signal[E], s2: Signal[V]): Signal[(E,V)] = new ZipSignal[E,V](s1, s2)

  def throttled[E](s: Signal[E], delay: FiniteDuration): Signal[E] = new ThrottlingSignal(s, delay)

  def mix[E](sources: Signal[_]*)(f: => E): Signal[E] = new ProxySignal(sources: _*)(Some(f))
  def foldLeft[E,V](sources: Signal[E]*)(v: V)(f: (V, E) => V): Signal[V] = new FoldLeftSignal[E,V](sources: _*)(v)(f)
  def and(sources: Signal[Boolean]*): Signal[Boolean] = new FoldLeftSignal[Boolean, Boolean](sources: _*)(true)(_ && _)
  def or(sources: Signal[Boolean]*): Signal[Boolean] = new FoldLeftSignal[Boolean, Boolean](sources: _*)(true)(_ || _)

  def const[E](value: E): Signal[E] = new ConstantSignal[E](value)

  def future[E](future: Future[E]): Signal[E] = returning(new Signal[E]()) { signal =>
    future.onSuccess { case res => signal.dispatch(res, Some(Threading.global)) } (Threading.global)
  }

  def wrap[E](source: Publisher[E]): Signal[E] = new Signal[E]() {
    override protected def onWire(): Unit = source += this
    override protected def onUnwire(): Unit = source -= this
  }

  implicit def signal_to_value[A](s: Signal[A]): A = s.get
}

class Signal[E]() extends Publisher[E] {
  @volatile protected[events] var value: Option[E] = None

  protected val cachingDisabled = false

  private val lock = new Object
  @volatile protected[events] var wired = false
  @volatile protected[events] var autoWiring = true

  protected lazy val _changed = Publisher[E](executionContext)

  def currentValue: Option[E] = {
    if (!wired) {
      warn(s"Accessing value of unwired signal ($this: $value), autowiring will be disabled")(s"Signal[]")
      disableAutowiring()
    }
    value
  }

  def get = currentValue.get

  def onChanged(s: => Unit)(implicit ec: EventContext): EventObserver[E] = onChanged((_: E) => s)(ec)

  def onChanged(s: Events.Subscriber[E])(implicit ec: EventContext): EventObserver[E] = {
    if (!wired) wire()
    _changed(s)(ec)
  }

  override def subscribe(subscriber: EventObserver[E]): Unit = {
    if (!wired) wire()
    super.subscribe(subscriber)
    value foreach { v =>
      if (executionContext.isEmpty) subscriber.apply(v)
      else Future { subscriber.apply(v) } (executionContext.get)
    }
  }

  override def unsubscribe(subscriber: EventObserver[E]): Unit = {
    super.unsubscribe(subscriber)

    if (autoWiring && !hasSubscribers && !_changed.hasSubscribers && wired) {
      unwire()
    }
  }

  def foreach(op: E => Unit)(implicit context: EventContext): Unit = apply(op)(context)

  override def dispatch(event: E, sourceContext: Option[ExecutionContext]): Unit = {
    if (cachingDisabled || this.value != Some(event)) {
      this.value = Some(event)
      _changed ! event
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

  def combine[V, T](s: Signal[V])(f: (E, V) => T): Signal[T] = new ProxySignal[T](this, s)(for (v <- value; v1 <- s.value) yield f(v, v1))

  def throttle(delay: FiniteDuration): Signal[E] = new ThrottlingSignal(this, delay)
}

trait Wired { self: Signal[_] =>
  self.disableAutowiring()
}

class ConstantSignal[E](v: E) extends Signal[E] {
  value = Some(v)
}

class ThrottlingSignal[E](source: Signal[E], delay: FiniteDuration) extends Signal[E] {
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
    if (!value.contains(event)) {
      value = Some(event)
      val time = System.currentTimeMillis()
      val context = sourceContext.getOrElse(Threading.global)
      if (lastDispatched < time) {
        val d = math.max(0, lastDispatched + delay.toMillis - time)
        lastDispatched = time + d
        CancellableFuture.delayed(d.millis) {
          ThrottlingSignal.this.value foreach { super.dispatch(_, Some(context)) }
        } (context)
      }
    }
  }
}

class FlatMapSignal[E, V](source: Signal[E], f: E => Signal[V]) extends Signal[V]() {
  @volatile private var mapped = source.value map f // TODO: what about threading, shouldn't f be executed on specific execution context
  value = mapped.flatMap(_.value)

  private val sourceDelegate = new Publisher[E] {
    protected[events] override def dispatch(v: E, sourceContext: Option[ExecutionContext]): Unit = setupMapped(f(v), sourceContext)
    override def hasSubscribers: Boolean = FlatMapSignal.this.hasSubscribers
    override def unsubscribeAll(): Unit = FlatMapSignal.this.unsubscribeAll()
  }

  override def onWire(): Unit = {
    source += sourceDelegate
    mapped.foreach(_ += this)

    value = mapped.flatMap(_.value)
    source.value.map(f).foreach(setupMapped(_, None))
  }

  override def onUnwire(): Unit = {
    source -= sourceDelegate
    mapped.foreach(_ -= this)
  }

  private def setupMapped(signal: Signal[V], sourceContext: Option[ExecutionContext]): Unit = {
    if (!mapped.contains(signal)) {
      mapped.foreach(_ -= this)
      mapped = Some(signal)

      signal += this
      signal.value.foreach(dispatch(_, sourceContext))
    }
  }
}

class ProxySignal[E](sources: Signal[_]*)(f: => Option[E]) extends Signal[E] {
  value = f

  private val delegate = new Publisher[Any] {
    protected[events] override def dispatch(event: Any, sourceContext: Option[ExecutionContext]): Unit = {
      f foreach { v => ProxySignal.this.dispatch(v, sourceContext) }
    }
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

class MapSignal[E, V](source: Signal[E], f: E => V) extends ProxySignal[V](source)(source.value map f)

class ZipSignal[E, V](s1: Signal[E], s2: Signal[V]) extends ProxySignal[(E, V)](s1, s2)(for (v1 <- s1.value; v2 <- s2.value) yield (v1, v2))

class FoldLeftSignal[E, V](sources: Signal[E]*)(v: V)(f: (V, E) => V) extends ProxySignal[V](sources: _*)({
  val values = sources.flatMap(_.value)
  if (values.size == sources.size) Some(values.foldLeft(v)(f)) else None
})
