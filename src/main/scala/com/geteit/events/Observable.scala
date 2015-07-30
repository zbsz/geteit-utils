package com.geteit.events

trait Observable[Listener] {

  private object listenersMonitor
  private var autowiring = true
  @volatile private[events] var wired = false
  @volatile private var listeners = Set.empty[Listener]

  protected def onWire(): Unit
  protected def onUnwire(): Unit

  private[events] def subscribe(l: Listener): Unit = listenersMonitor.synchronized {
    listeners += l
    if (!wired) {
      wired = true
      onWire()
    }
  }

  private[events] def unsubscribe(l: Listener): Unit = listenersMonitor.synchronized {
    listeners -= l
    if (wired && autowiring && listeners.isEmpty) {
      wired = false
      onUnwire()
    }
  }

  private[events] def notifyListeners(invoke: Listener => Unit): Unit = listeners foreach invoke

  def hasSubscribers = listeners.nonEmpty

  def unsubscribeAll() = listenersMonitor.synchronized {
    listeners = Set.empty
    if (wired && autowiring) {
      wired = false
      onUnwire()
    }
  }

  def disableAutowiring() = listenersMonitor.synchronized {
    autowiring = false
    if (!wired) {
      wired = true
      onWire()
    }
  }
}
