package com.geteit.events

import com.geteit.util.LoggedTry

import scala.concurrent.{ExecutionContext, Future}

trait EventPublisher[-E] {
  protected[events] def dispatch(event: E, sourceContext: Option[ExecutionContext]): Unit

  def publish(event: E): Unit = dispatch(event, None)

  def !(event: E): Unit = publish(event)
}

object Publisher {
  def apply[E](ec: Option[ExecutionContext]) = new Publisher[E] {
    override val executionContext = ec
  }
}

class Publisher[E] extends EventSource[E] with EventPublisher[E] {
  private object subscribersLock
  private object childrenLock

  @volatile private var subscribers: Set[EventObserver[E]] = Set.empty
  @volatile private var children: Set[Publisher[_ >: E]] = Set.empty

  /** Add a subscriber to our list if it is not already there. */
  protected[events] def subscribe(subscriber: EventObserver[E]): Unit = subscribersLock.synchronized(subscribers += subscriber)

  /** Remove a subscriber from our list.  If not in the list, ignored. */
  protected[events] def unsubscribe(subscriber: EventObserver[E]): Unit = subscribersLock.synchronized(subscribers -= subscriber)

  private[events] def +=(child: Publisher[_ >: E]): Unit = childrenLock.synchronized(children += child)
  private[events] def -=(child: Publisher[_ >: E]): Unit = childrenLock.synchronized(children -= child)

  def unsubscribeAll(): Unit = {
    subscribersLock.synchronized {
      val previous = subscribers
      subscribers = Set.empty
      previous
    } foreach (_.destroy())
    children.foreach(_.unsubscribeAll())
  }

  def hasSubscribers: Boolean = subscribers.nonEmpty || children.exists(_.hasSubscribers)

  protected[events] def dispatchEvent(event: E, currentExecutionContext: Option[ExecutionContext]): Unit = {
    children.foreach(_.dispatch(event, currentExecutionContext))
    subscribers.foreach(_.apply(event))
  }

  protected[events] def dispatch(event: E, sourceContext: Option[ExecutionContext]): Unit = executionContext match {
    case None | `sourceContext` => dispatchEvent(event, sourceContext)
    case Some(ctx) => Future(dispatchEvent(event, executionContext))(ctx)
  }

  def contramap[T](f: T => E): Publisher[T] = new Publisher[T] {
    protected[events] override def dispatch(event: T, sourceContext: Option[ExecutionContext]): Unit = Publisher.this.dispatch(f(event), sourceContext)
  }
}
