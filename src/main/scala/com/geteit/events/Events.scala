package com.geteit.events

import com.geteit.concurrent.Threading
import com.geteit.util.{LoggedTry, returning}

import scala.concurrent.{ExecutionContext, Future}
import scala.ref.WeakReference

object Events {
  type Subscriber[-E] = E => Unit

  def UiExecutionContext = Threading.ui
  def BgExecutionContext = Threading.global

  def removeObserver[T <: AnyRef](xs: Vector[T], x: T): Vector[T] = {
    val (pre, post) = xs.span(_ ne x)
    if (post.isEmpty) pre else pre ++ post.tail
  }
}

trait Subscription {
  def enable(): Unit
  def disable(): Unit
  def destroy(): Unit
  def disablePauseWithContext(): Unit

  private[events] def subscribe(): Unit
  private[events] def unsubscribe(): Unit
}

trait EventSource[E] {
  val executionContext = Option.empty[ExecutionContext]

  def on(ec: ExecutionContext)(subscriber: Events.Subscriber[E])(implicit context: EventContext): Subscription
  def apply(subscriber: Events.Subscriber[E])(implicit context: EventContext): Subscription
}

trait ForcedEventSource[E] extends EventSource[E] {
  abstract override def on(ec: ExecutionContext)(subscriber: Events.Subscriber[E])(implicit context: EventContext): Subscription = returning(super.on(ec)(subscriber))(_.disablePauseWithContext())
  abstract override def apply(subscriber: Events.Subscriber[E])(implicit context: EventContext): Subscription = returning(super.apply(subscriber))(_.disablePauseWithContext())
}

abstract class BaseSubscription(context: WeakReference[EventContext]) extends Subscription {
  @volatile protected[events] var subscribed = false
  private var enabled = false
  private var pauseWithContext = true

  context.get foreach (_.register(this))

  protected[events] def onSubscribe(): Unit
  protected[events] def onUnsubscribe(): Unit

  private[events] def subscribe(): Unit = {
    if (enabled && !subscribed) {
      subscribed = true
      onSubscribe()
    }
  }

  private[events] def unsubscribe(): Unit = {
    if (subscribed && (pauseWithContext || !enabled)) {
      subscribed = false
      onUnsubscribe()
    }
  }

  def enable(): Unit =
    context.get foreach { context =>
      enabled = true
      if (context.isContextStarted) subscribe()
    }

  def disable(): Unit = {
    enabled = false
    if (subscribed) unsubscribe()
  }

  def destroy(): Unit = {
    disable()
    context.get foreach (_.unregister(this))
  }

  def disablePauseWithContext(): Unit = {
    pauseWithContext = false
    subscribe()
  }
}

class SignalSubscription[E](source: Signal[E], subscriber: Events.Subscriber[E], executionContext: Option[ExecutionContext] = None)(implicit context: WeakReference[EventContext]) extends BaseSubscription(context) with SignalListener {
  private val contextSwitch = executionContext.exists(ec => !source.executionContext.contains(ec))
  private var prev: E = _

  override def changed(ec: Option[ExecutionContext]): Unit = synchronized {
    source.value foreach { event =>
      if (event != prev) {
        prev = event
        if (subscribed) {
          if (contextSwitch) Future { if (subscribed) LoggedTry(subscriber(event))("SignalSubscription") }(executionContext.get)
          else subscriber(event)
        }
      }
    }
  }

  override protected[events] def onSubscribe(): Unit = {
    source.subscribe(this)
    changed(None) // refresh listener with current value
  }

  override protected[events] def onUnsubscribe(): Unit = source.unsubscribe(this)
}

class StreamSubscription[E](source: EventStream[E], subscriber: Events.Subscriber[E], executionContext: Option[ExecutionContext] = None)(implicit context: WeakReference[EventContext]) extends BaseSubscription(context) with EventListener[E] {
  private val contextSwitch = executionContext.exists(ec => !source.executionContext.contains(ec))

  override def onEvent(event: E, ec: Option[ExecutionContext]): Unit = {
    if (subscribed) {
      if (contextSwitch) Future { if (subscribed) LoggedTry(subscriber(event))("StreamSubscription") } (executionContext.get)
      else subscriber(event)
    }
  }

  override protected[events] def onSubscribe(): Unit = source.subscribe(this)

  override protected[events] def onUnsubscribe(): Unit = source.unsubscribe(this)
}

trait BgEventSource { self: EventSource[_] =>
  override val executionContext = Some(Events.BgExecutionContext)
}

trait UiEventSource { self: EventSource[_] =>
  override val executionContext = Some(Events.UiExecutionContext)
}
