package com.geteit.events

import android.app.Service
import android.support.v4.app.Fragment
import android.support.v4.content.Loader
import android.view.View
import com.geteit.util.Log._

import scala.ref.WeakReference

trait EventContext {
  private implicit val logTag: LogTag = logTagFor[EventContext]

  private object lock

  private var started = false
  private var destroyed = false
  private var observers: Vector[Subscription] = Vector.empty
  implicit val eventContext = this

  implicit def weakEventContext: WeakReference[EventContext] = WeakReference(this)

  override protected def finalize(): Unit = {
    lock.synchronized { if (!destroyed) onContextDestroy() }
    super.finalize()
  }

  def onContextStart(): Unit = {
    lock.synchronized {
      if (!started) {
        started = true
        observers foreach (_.subscribe())
      }
    }
  }

  def onContextStop(): Unit = {
    lock.synchronized {
      if (started) {
        started = false
        observers foreach (_.unsubscribe())
      }
    }
  }

  def onContextDestroy(): Unit = {
    lock.synchronized {
      destroyed = true
      val os = observers
      observers = Vector.empty
      os foreach (_.destroy())
    }
  }

  private[events] def register(observer: Subscription): Unit = {
    lock.synchronized {
      assert(!destroyed, "context already destroyed")

      if (!observers.contains(observer)) {
        observers +:= observer
        if (started) observer.subscribe()
      }
    }
  }

  private[events] def unregister(observer: Subscription): Unit = {
    lock.synchronized { observers = Events.removeObserver(observers, observer) }
  }

  def isContextStarted: Boolean = lock.synchronized(started && !destroyed)
}

object EventContext {

  object Implicits {
    implicit val global: EventContext = EventContext.Global
  }

  val Global: EventContext = new EventContext {
    override private[events] def register(observer: Subscription): Unit = () // do nothing, global context will never need the observers (can not be stopped)
    override private[events] def unregister(observer: Subscription): Unit = ()
    override def onContextStart(): Unit = ()
    override def onContextStop(): Unit = ()
    override def onContextDestroy(): Unit = ()
    override def isContextStarted: Boolean = true
  }
}

trait FragmentEventContext extends Fragment with EventContext {

  override def onResume(): Unit = {
    onContextStart()
    super.onResume()
  }

  override def onPause(): Unit = {
    super.onPause()
    onContextStop()
  }

  override def onDestroy(): Unit = {
    super.onDestroy()
    onContextDestroy()
  }
}

trait ViewEventContext extends View with EventContext {

  private var attached = false

  override def onAttachedToWindow(): Unit = {
    super.onAttachedToWindow()

    attached = true
    if (getVisibility != View.GONE) onContextStart()
  }

  override def setVisibility(visibility: Int): Unit = {
    super.setVisibility(visibility)

    if (visibility != View.GONE && attached) onContextStart()
    else onContextStop()
  }

  override def onDetachedFromWindow(): Unit = {
    super.onDetachedFromWindow()

    attached = false
    onContextStop()
  }
}

trait LoaderEventContext[T] extends Loader[T] with EventContext {

  override def onStartLoading(): Unit = {
    onContextStart()
    super.onStartLoading()
  }

  override def onAbandon(): Unit = {
    super.onAbandon()
    onContextStop()
  }
}

trait ServiceEventContext extends Service with EventContext {

  override def onCreate(): Unit = {
    super.onCreate()
    onContextStart()
  }

  override def onDestroy(): Unit = {
    onContextStop()
    onContextDestroy()
    super.onDestroy()
  }
}

object Cancellable {
  def apply[T](body: Cancellable => T) = {
    body(new Cancellable)
  }
}

class Cancellable {
  private var observers: List[Subscription] = Nil

  var cancelled = false
  val onCancel = EventStream[Cancellable]()

  def ::=(o: Subscription): Unit = {
    observers ::= o
  }

  def cancel(): Unit = {
    cancelled = true
    onCancel ! this
    observers foreach (_.destroy())
    onCancel.unsubscribeAll()
  }
}

