package com.geteit.events

import com.geteit.concurrent.{CancellableFuture, Threading}
import com.geteit.util._
import Log._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import com.geteit.util._

object Signal {
  import language.implicitConversions

  def empty[A] = new Signal[A]
  def apply[A](e: A) = new Signal[A](Some(e))
  def apply[A, B](s1: Signal[A], s2: Signal[B]): Signal[(A, B)] = new ZipSignal[A ,B](s1, s2)

  def throttled[A](s: Signal[A], delay: FiniteDuration): Signal[A] = new ThrottlingSignal(s, delay)

  def mix[A](sources: Signal[_]*)(f: => Option[A]): Signal[A] = new ProxySignal(sources: _*)(f)
  def foldLeft[A, B](sources: Signal[A]*)(v: B)(f: (B, A) => B): Signal[B] = new FoldLeftSignal[A,B](sources: _*)(v)(f)
  def and(sources: Signal[Boolean]*): Signal[Boolean] = new FoldLeftSignal[Boolean, Boolean](sources: _*)(true)(_ && _)
  def or(sources: Signal[Boolean]*): Signal[Boolean] = new FoldLeftSignal[Boolean, Boolean](sources: _*)(false)(_ || _)

  def const[E](value: E): Signal[E] = apply(value)

  def future[E](future: Future[E]): Signal[E] = returning(new Signal[E]()) { signal =>
    future.onSuccess { case res => signal.dispatch(res, Some(Threading.global)) } (Threading.global)
  }

  def wrap[A](initial: A, source: Publisher[A]): Signal[A] = new Signal[A](Some(initial)) {
    override protected def onWire(): Unit = {
      source += this
      valueMonitor.synchronized(value = Some(initial))
    }
    override protected def onUnwire(): Unit = source -= this
  }

  implicit def signal_to_value[A](s: Signal[A]): A = s.get
}

class Signal[A] extends Publisher[A] {
  protected object valueMonitor
  @volatile protected[events] var value: Option[A] = None

  protected object wiringMonitor
  @volatile private var wired = false
  @volatile private var autoWiring = true

  def this(a: Option[A]) = {
    this()
    value = a
  }

  protected val cachingDisabled = false

  protected lazy val _changed = Publisher[A](executionContext)

  def onChanged(s: => Unit)(implicit ec: EventContext): EventObserver[A] = onChanged((_: A) => s)(ec)

  def onChanged(s: Events.Subscriber[A])(implicit ec: EventContext): EventObserver[A] = {
    if (! wired) wire()
    _changed(s)(ec)
  }

  final def currentValue: Option[A] = {
    if (! wired) {
      warn(s"Accessing value of unwired signal ($this: $value), autowiring will be disabled")(s"Signal[]")
      disableAutowiring()
    }
    value
  }

  def get = currentValue.get

  override def dispatch(next: A, sourceContext: Option[ExecutionContext]): Unit = valueMonitor synchronized {
    if (cachingDisabled || !value.contains(next)) {
      value = Some(next)
      _changed ! next
      super.dispatch(next, sourceContext)
    }
  }

  def mutate(f: A => A): Unit = valueMonitor synchronized (value foreach (v => dispatch(f(v), None)))

  def zip[B](s: Signal[B]): Signal[(A, B)] = new ZipSignal[A, B](this, s)
  def map[B](f: A => B): Signal[B] = new MapSignal[A, B](this, f)
  def filter(f: A => Boolean): Signal[A] = new FilterSignal(this, f)
  def flatMap[B](f: A => Signal[B]): Signal[B] = new FlatMapSignal[A, B](this, f)
  def combine[B, C](s: Signal[B])(f: (A, B) => C): Signal[C] = new ProxySignal[C](this, s)(for (a <- this.value; b <- s.value) yield f(a, b))
  def throttle(delay: FiniteDuration): Signal[A] = new ThrottlingSignal(this, delay)

  /** If this signal is computed from sources that change their value via a side effect (such as signals) and is not
    * informed of those changes while unwired (e.g. because this signal removes itself from the sources' children
    * lists in #onUnwire), it is mandatory to update/recompute this signal's value from the sources in #onWire, since
    * a dispatch always happens after #onWire. This is true even if the source values themselves did not change, for the
    * recomputation in itself may rely on side effects (e.g. ZMessaging => SomeValueFromTheDatabase).
    *
    * This also implies that a signal should never #dispatch in #onWire because that will happen anyway immediately
    * afterwards in #subscribe.
    */
  protected def onWire(): Unit = ()
  protected def onUnwire(): Unit = ()

  protected def wire(): Unit = wiringMonitor.synchronized {
    if (! wired) {
      wired = true // XXX: this has to be before onWire to prevent stack overflow (onWire -> currentValue -> disableAutowiring -> wire -> onWire)
      onWire()
    }
  }

  protected def unwire(): Unit = wiringMonitor.synchronized {
    if (wired) {
      onUnwire()
      wired = false
    }
  }

  def disableAutowiring(): Unit = {
    autoWiring = false
    if (! wired) wire()
  }

  override private[events] def +=(child: Publisher[_ >: A]): Unit = {
    if (! wired) wire()
    super.+=(child)
  }

  override private[events] def -=(child: Publisher[_ >: A]): Unit = {
    super.-=(child)
    if (autoWiring && ! hasSubscribers && ! _changed.hasSubscribers && wired) {
      unwire()
    }
  }

  override def subscribe(subscriber: EventObserver[A]): Unit = {
    if (! wired) wire()
    super.subscribe(subscriber)

    value.foreach { c =>
      if (executionContext.isEmpty) subscriber.apply(c)
      else Future(subscriber.apply(c))(executionContext.get)
    }
  }

  override def unsubscribe(subscriber: EventObserver[A]): Unit = {
    super.unsubscribe(subscriber)

    if (autoWiring && ! hasSubscribers && ! _changed.hasSubscribers && wired) {
      unwire()
    }
  }
}
trait Wired { self: Signal[_] =>
  self.disableAutowiring()
}

class ThrottlingSignal[A](source: Signal[A], delay: FiniteDuration) extends Signal[A] {
  import scala.concurrent.duration._
  override protected val cachingDisabled: Boolean = true
  @volatile private var lastDispatched = 0L

  override def onWire(): Unit = {
    source += this
    valueMonitor.synchronized {
      val next = source.value
      if (next.isDefined) value = next
    }
  }

  override def onUnwire(): Unit = {
    source -= this
  }

  override def dispatch(next: A, sourceContext: Option[ExecutionContext]): Unit = valueMonitor synchronized {
    if (!value.contains(next)) {
      value = Some(next)
      val time = System.currentTimeMillis()
      val context = sourceContext.getOrElse(Threading.global)
      if (lastDispatched < time) {
        val d = math.max(0, lastDispatched - time + delay.toMillis)
        lastDispatched = time + d
        CancellableFuture.delayed(d.millis)(value.foreach(super.dispatch(_, Some(context))))(context)
      }
    }
  }
}

class FlatMapSignal[A, B](source: Signal[A], f: A => Signal[B]) extends Signal[B] {
  @volatile private var sourceValue: Option[A] = None
  @volatile private var mapped = Signal.empty[B]

  private val sourceDelegate = new Publisher[A] {
    protected[events] override def dispatch(nextSource: A, sourceContext: Option[ExecutionContext]): Unit = {
      val changed = wiringMonitor synchronized {
        if (!sourceValue.contains(nextSource)) {
          sourceValue = Some(nextSource)

          mapped -= FlatMapSignal.this
          mapped = f(nextSource)
          mapped += FlatMapSignal.this
          true
        } else false
      }

      if (changed) mapped.value.foreach(v => FlatMapSignal.this.dispatch(v, None))
    }

    override def hasSubscribers: Boolean = FlatMapSignal.this.hasSubscribers
    override def unsubscribeAll(): Unit = FlatMapSignal.this.unsubscribeAll()
  }

  override def onWire(): Unit = {
    source += sourceDelegate

    val nextSource = source.value
    if (sourceValue != nextSource) {
      sourceValue = nextSource
      mapped = nextSource.fold(Signal.empty[B])(f)
    }

    mapped += this
    valueMonitor.synchronized {
      val next = mapped.value
      if (next.isDefined) value = next
    }
  }

  override def onUnwire(): Unit = {
    source -= sourceDelegate
    mapped -= this
  }
}

class FilterSignal[A](source: Signal[A], f: A => Boolean) extends Signal[A] {
  override def onWire(): Unit = {
    source += this
    valueMonitor.synchronized {
      val v = source.value
      if (v.exists(f)) value = v
    }
  }

  override def onUnwire(): Unit = {
    source -= this
  }

  override def dispatch(next: A, sourceContext: Option[ExecutionContext]): Unit = valueMonitor synchronized {
    if (!value.contains(next) && f(next)) super.dispatch(next, sourceContext)
  }
}

class ProxySignal[A](sources: Signal[_]*)(f: => Option[A]) extends Signal[A] {
  private val delegate = new Publisher[Any] {
    protected[events] override def dispatch(event: Any, sourceContext: Option[ExecutionContext]): Unit =
      f foreach (ProxySignal.this.dispatch(_, sourceContext))

    override def hasSubscribers: Boolean = ProxySignal.this.hasSubscribers
    override def unsubscribeAll(): Unit = ProxySignal.this.unsubscribeAll()
  }

  override def onWire(): Unit = {
    sources foreach (_ += delegate)
    valueMonitor.synchronized {
      val next = f
      if (next.isDefined) value = next
    }
  }

  override def onUnwire(): Unit = sources foreach (_ -= delegate)
}

class MapSignal[A, B](source: Signal[A], f: A => B) extends ProxySignal[B](source)(source.value map f)

class ZipSignal[A, B](s1: Signal[A], s2: Signal[B]) extends ProxySignal[(A, B)](s1, s2)(for (a <- s1.value; b <- s2.value) yield (a, b))

class FoldLeftSignal[A, B](sources: Signal[A]*)(v: B)(f: (B, A) => B) extends ProxySignal[B](sources: _*)(
  sources.foldLeft(Option(v))((mv, signal) => for (a <- mv; b <- signal.value) yield f(a, b))
)

