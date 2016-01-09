package com.geteit.events

import java.util.concurrent.atomic.AtomicBoolean

import com.geteit.concurrent.{CancellableFuture, Threading}
import com.geteit.events.Events.Subscriber
import com.geteit.util.Log._
import com.geteit.util._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Promise, ExecutionContext, Future}
import scala.ref.WeakReference

object Signal {
  private implicit val logTag: LogTag = "Signal"
  import language.implicitConversions

  def apply[A]() = new SourceSignal[A]
  def apply[A](e: A) = new SourceSignal[A](Some(e))
  def empty[A]: Signal[A] = new ConstSignal[A](None)
  def const[A](v: A): Signal[A] = new ConstSignal[A](Some(v))
  def apply[A, B](s1: Signal[A], s2: Signal[B]): Signal[(A, B)] = new ZipSignal[A ,B](s1, s2)
  def apply[A, B, C](s1: Signal[A], s2: Signal[B], s3: Signal[C]): Signal[(A, B, C)] = new Zip3Signal[A ,B, C](s1, s2, s3)
  def apply[A, B, C, D](s1: Signal[A], s2: Signal[B], s3: Signal[C], s4: Signal[D]): Signal[(A, B, C, D)] = new Zip4Signal[A ,B, C, D](s1, s2, s3, s4)

  def throttled[A](s: Signal[A], delay: FiniteDuration): Signal[A] = new ThrottlingSignal(s, delay)

  def mix[A](sources: Signal[_]*)(f: => Option[A]): Signal[A] = new ProxySignal[A](sources: _*) {
    override protected def computeValue(current: Option[A]): Option[A] = f
  }
  def foldLeft[A, B](sources: Signal[A]*)(v: B)(f: (B, A) => B): Signal[B] = new FoldLeftSignal[A,B](sources: _*)(v)(f)
  def sequence[A](sources: Seq[Signal[A]]): Signal[Seq[A]] = new SequenceSignal(sources)
  def and(sources: Signal[Boolean]*): Signal[Boolean] = new FoldLeftSignal[Boolean, Boolean](sources: _*)(true)(_ && _)
  def or(sources: Signal[Boolean]*): Signal[Boolean] = new FoldLeftSignal[Boolean, Boolean](sources: _*)(false)(_ || _)

  def future[A](future: Future[A]): Signal[A] = returning(new Signal[A]) { signal =>
    future.onSuccess {
      case res => signal.set(Option(res), Some(Threading.global))
    } (Threading.global)
  }

  def wrap[A](source: EventStream[A]): Signal[A] = new Signal[A](None) with EventListener[A] {
    override protected[events] def onEvent(event: A, ec: Option[ExecutionContext]): Unit = set(Some(event), ec)
    override protected def onWire(): Unit = source.subscribe(this)
    override protected def onUnwire(): Unit = source.subscribe(this)
  }

  def scan[A, B](source: EventStream[A], zero: B)(f: (B, A) => B): Signal[B] = new ScanSignal(source, zero)(f)

  implicit def signal_to_value[A](s: Signal[A]): A = s.get
}

trait Source[A] { self: Signal[A] =>
  def ! (value: A) = try set(Some(value)) catch { case t: Throwable => error("signal processing failed", t)("SourceSignal") }
  override def publish(value: A, currentContext: ExecutionContext): Unit =
    try set(Some(value), Option(currentContext)) catch { case t: Throwable => error("signal processing failed", t)("SourceSignal") }

  def mutate(f: A => A): Unit = update(_.map(f))
  def mutate(f: A => A, default: => A): Unit = update(_.map(f).orElse(Some(default)))
}

class SourceSignal[A](v: Option[A] = None) extends Signal(v) with Source[A]

trait SignalListener {
  def changed(ec: Option[ExecutionContext]): Unit
}

class Signal[A](@volatile var value: Option[A] = None) extends Observable[SignalListener] with EventSource[A] { self =>
  import Signal.logTag

  private object updateMonitor

  private[events] def update(f: Option[A] => Option[A], ec: Option[ExecutionContext] = None, notify: Boolean = true): Unit = {
    val changed = updateMonitor.synchronized {
      val next = f(value)
      if (value != next) { value = next; true }
      else false
    }
    if (changed && notify) notifyListeners(ec)
  }

  private[events] def set(v: Option[A], ec: Option[ExecutionContext] = None) =
    if (value != v) {
      value = v
      notifyListeners(ec)
    }

  private[events] def notifyListeners(ec: Option[ExecutionContext]): Unit = super.notifyListeners { _.changed(ec) }

  final def currentValue: Option[A] = {
    if (!wired) {
      warn("Accessing value of unwired signal, autowiring will be disabled")
      disableAutowiring()
    }
    value
  }

  lazy val onChanged = new EventStream[A] with SignalListener { stream =>
    override def changed(ec: Option[ExecutionContext]): Unit = stream.synchronized { self.value.foreach(v => dispatch(v, ec)) }
    override protected def onWire(): Unit = self.subscribe(stream)
    override protected def onUnwire(): Unit = self.unsubscribe(stream)
  }

  def get = currentValue.get

  def head(implicit ev: EventContext) = value match {
    case Some(v) => CancellableFuture successful v
    case None =>
      val p = Promise[A]()
      val o = apply { p.trySuccess(_) }
      p.future.onComplete(_ => o.destroy())(Threading.global)
      new CancellableFuture(p)
  }

  def clear() = set(None)

  def zip[B](s: Signal[B]): Signal[(A, B)] = new ZipSignal[A, B](this, s)
  def map[B](f: A => B): Signal[B] = new MapSignal[A, B](this, f)
  def filter(f: A => Boolean): Signal[A] = new FilterSignal(this, f)
  def collect[B](pf: PartialFunction[A, B]): Signal[B] = new ProxySignal[B](this) {
    override protected def computeValue(current: Option[B]): Option[B] = self.value flatMap { v =>
      pf.andThen(Some(_)).applyOrElse(v, { _: A => None })
    }
  }
  def flatMap[B](f: A => Signal[B]): Signal[B] = new FlatMapSignal[A, B](this, f)
  def combine[B, C](s: Signal[B])(f: (A, B) => C): Signal[C] = new ProxySignal[C](this, s) {
    override protected def computeValue(current: Option[C]): Option[C] = for (a <- self.value; b <- s.value) yield f(a, b)
  }
  def throttle(delay: FiniteDuration): Signal[A] = new ThrottlingSignal(this, delay)

  def orElse(fallback: Signal[A]): Signal[A] = new ProxySignal[A](self, fallback) {
    override protected def computeValue(current: Option[A]): Option[A] = self.value.orElse(fallback.value)
  }

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

  override def on(ec: ExecutionContext)(subscriber: Subscriber[A])(implicit eventContext: EventContext): Subscription = returning(new SignalSubscription[A](this, subscriber, Some(ec))(WeakReference(eventContext)))(_.enable())
  override def apply(subscriber: Subscriber[A])(implicit eventContext: EventContext): Subscription = returning(new SignalSubscription[A](this, subscriber, executionContext)(WeakReference(eventContext)))(_.enable())

  protected def publish(value: A): Unit = set(Some(value))
  protected def publish(value: A, currentContext: ExecutionContext): Unit = set(Some(value), Some(currentContext))
}

/**
 * Immutable signal value. Can be used whenever some constant or empty signal is needed.
 * Using immutable signals in flatMap chains should have better performance compared to regular signals with the same value.
 */
class ConstSignal[A](v: Option[A]) extends Signal[A](v) {
  override def subscribe(l: SignalListener): Unit = ()
  override def unsubscribe(l: SignalListener): Unit = ()
  override private[events] def update(f: (Option[A]) => Option[A], ec: Option[ExecutionContext], notify: Boolean = true): Unit = throw new UnsupportedOperationException("Const signal can not be updated")
  override private[events] def set(v: Option[A], ec: Option[ExecutionContext]): Unit = throw new UnsupportedOperationException("Const signal can not be changed")
}

class ThrottlingSignal[A](source: Signal[A], delay: FiniteDuration) extends ProxySignal[A](source) {
  import scala.concurrent.duration._
  private val waiting = new AtomicBoolean(false)
  @volatile private var lastDispatched = 0L

  override protected def computeValue(current: Option[A]): Option[A] = source.value

  override private[events] def notifyListeners(ec: Option[ExecutionContext]): Unit =
    if (waiting.compareAndSet(false, true)) {
      val context = ec.getOrElse(Threading.global)
      val d = math.max(0, lastDispatched - System.currentTimeMillis() + delay.toMillis)
      CancellableFuture.delayed(d.millis) {
        lastDispatched = System.currentTimeMillis()
        waiting.set(false)
        super.notifyListeners(Some(context))
      } (context)
    }
}

class FlatMapSignal[A, B](source: Signal[A], f: A => Signal[B]) extends Signal[B] with SignalListener {
  private val Empty = Signal.empty[B]

  private object wiringMonitor
  private var sourceValue: Option[A] = None
  private var mapped: Signal[B] = Empty

  private val sourceListener = new SignalListener {
    override def changed(ec: Option[ExecutionContext]): Unit = {
      val changed = wiringMonitor synchronized { // XXX: is this synchronization needed, is it enough? What if we just got unwired ?
        val next = source.value
        if (sourceValue != next) {
          sourceValue = next

          mapped.unsubscribe(FlatMapSignal.this)
          mapped = next.map(f).getOrElse(Empty)
          mapped.subscribe(FlatMapSignal.this)
          true
        } else false
      }

      if (changed) set(mapped.value)
    }
  }

  override def onWire(): Unit = wiringMonitor.synchronized {
    source.subscribe(sourceListener)

    val next = source.value
    if (sourceValue != next) {
      sourceValue = next
      mapped = next.map(f).getOrElse(Empty)
    }

    mapped.subscribe(this)
    value = mapped.value
  }

  override def onUnwire(): Unit = wiringMonitor.synchronized {
    source.unsubscribe(sourceListener)
    mapped.unsubscribe(this)
  }

  override def changed(ec: Option[ExecutionContext]): Unit = set(mapped.value, ec)
}

class ScanSignal[A, B](source: EventStream[A], zero: B)(f: (B, A) => B) extends Signal[B](Some(zero)) with EventListener[A] {
  override protected[events] def onEvent(event: A, ec: Option[ExecutionContext]): Unit = update(_.map(f(_, event)), ec)
  override protected def onWire(): Unit = source.subscribe(this)
  override protected def onUnwire(): Unit = source.unsubscribe(this)
}

abstract class ProxySignal[A](sources: Signal[_]*) extends Signal[A] with SignalListener {
  override def onWire(): Unit = {
    sources foreach (_.subscribe(this))
    update(computeValue, notify = false)
  }

  override def onUnwire(): Unit = sources foreach (_.unsubscribe(this))

  override def changed(ec: Option[ExecutionContext]): Unit = update(computeValue, ec)

  protected def computeValue(current: Option[A]): Option[A]
}

class FilterSignal[A](source: Signal[A], f: A => Boolean) extends ProxySignal[A](source) {
  override protected def computeValue(current: Option[A]): Option[A] = source.value.filter(f)
}

class MapSignal[A, B](source: Signal[A], f: A => B) extends ProxySignal[B](source) {
  override protected def computeValue(current: Option[B]): Option[B] = source.value map f
}

class ZipSignal[A, B](s1: Signal[A], s2: Signal[B]) extends ProxySignal[(A, B)](s1, s2) {
  override protected def computeValue(current: Option[(A, B)]): Option[(A, B)] = for (a <- s1.value; b <- s2.value) yield (a, b)
}

class Zip3Signal[A, B, C](s1: Signal[A], s2: Signal[B], s3: Signal[C]) extends ProxySignal[(A, B, C)](s1, s2, s3) {
  override protected def computeValue(current: Option[(A, B, C)]): Option[(A, B, C)] = for (a <- s1.value; b <- s2.value; c <- s3.value) yield (a, b, c)
}

class Zip4Signal[A, B, C, D](s1: Signal[A], s2: Signal[B], s3: Signal[C], s4: Signal[D]) extends ProxySignal[(A, B, C, D)](s1, s2, s3, s4) {
  override protected def computeValue(current: Option[(A, B, C, D)]): Option[(A, B, C, D)] = for (a <- s1.value; b <- s2.value; c <- s3.value; d <- s4.value) yield (a, b, c, d)
}

class FoldLeftSignal[A, B](sources: Signal[A]*)(v: B)(f: (B, A) => B) extends ProxySignal[B](sources: _*) {
  override protected def computeValue(current: Option[B]): Option[B] =
    sources.foldLeft(Option(v))((mv, signal) => for (a <- mv; b <- signal.value) yield f(a, b))
}

class SequenceSignal[A](sources: Seq[Signal[A]]) extends ProxySignal[Seq[A]](sources: _*) {
  override protected def computeValue(current: Option[Seq[A]]): Option[Seq[A]] = {
    val b = Seq.newBuilder[A]
    var empty = false
    sources.foreach { s => if (!empty) s.value.fold[Any](empty = true)(b += _) }
    if (empty) None else Some(b.result())
  }
}
