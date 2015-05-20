package com.geteit.events

import com.geteit.concurrent.Threading

import scala.concurrent.{Future, ExecutionContext}
import scala.ref.WeakReference

object Events {
  type Subscriber[-E] = E => Unit

  def removeObserver[T <: AnyRef](xs: Vector[T], x: T): Vector[T] = {
    val (pre, post) = xs.span(_ ne x)
    if (post.isEmpty) pre else pre ++ post.tail
  }
}

trait EventSource[E] {
  val executionContext = Option.empty[ExecutionContext]

  protected[events] def subscribe(subscriber: EventObserver[E]): Unit
  protected[events] def unsubscribe(subscriber: EventObserver[E]): Unit

  def on(ec: ExecutionContext)(subscriber: Events.Subscriber[E])(implicit context: EventContext) = new EventObserver(this, subscriber, Some(ec))(WeakReference(context)).enable()
  def apply(subscriber: Events.Subscriber[E])(implicit context: EventContext) = new EventObserver(this, subscriber)(WeakReference(context)).enable()
}

trait ForcedEventSource[E] extends EventSource[E] {
  override def on(ec: ExecutionContext)(subscriber: Events.Subscriber[E])(implicit context: EventContext): EventObserver[E] = super.on(ec)(subscriber)(context).disablePauseWithContext()
  override def apply(subscriber: Events.Subscriber[E])(implicit context: EventContext): EventObserver[E] = super.apply(subscriber)(context).disablePauseWithContext()
}

class EventObserver[-E](source: EventSource[E], subscriber: Events.Subscriber[E], executionContext: Option[ExecutionContext] = None)(implicit context: WeakReference[EventContext]) extends (E => Unit) {
  private var subscribed = false
  private var enabled = false
  private var pauseWithContext = true
  private val contextSwitch = executionContext.exists(ec => source.executionContext.forall(_ != ec))

  context.get foreach (_.register(this))

  def apply(event: E): Unit = {
    if (subscribed) {
      if (contextSwitch) Future { if (subscribed) subscriber(event) } (executionContext.get)
      else subscriber(event)
    }
  }

  private[events] def subscribe(): Unit = {
    if (enabled && !subscribed) {
      subscribed = true
      source.subscribe(this)
    }
  }

  private[events] def unsubscribe(): Unit = {
    if (subscribed && (pauseWithContext || !enabled)) {
      subscribed = false
      source.unsubscribe(this)
    }
  }

  def enable(): EventObserver[E] = {
    context.get foreach { context =>
      enabled = true
      if (context.isContextStarted) subscribe()
    }
    this
  }

  def disable(): EventObserver[E] = {
    enabled = false
    if (subscribed) unsubscribe()
    this
  }

  def destroy(): Unit = {
    disable()
    context.get foreach (_.unregister(this))
  }

  def disablePauseWithContext(): EventObserver[E] = {
    pauseWithContext = false
    subscribe()
    this
  }
}

trait BgEventSource { self: EventSource[_] =>
  override val executionContext = Some(Threading.global)
}

trait UiEventSource { self: EventSource[_] =>
  override val executionContext = Some(Threading.ui)
}
