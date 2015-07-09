package com.geteit.app

import android.app.Application
import com.geteit.events.Publisher
import com.geteit.inject.{GtContextModule, Module}

object GtApplication {
  var APP_INSTANCE: GtApplication = null

  val onLowMemory = new Publisher[Null]

  val onTrimMemory = new Publisher[Int]
}

class GtApplication extends Application with GtContext {

  GtContext.Global = this
  GtApplication.APP_INSTANCE = this

  def contextModule(ctx: GtContext): Module = GtContextModule(ctx)

  override def onCreate() {
    super.onCreate()
    publishCreate()
  }

  override def onTerminate() {
    publishDestroy()
    super.onTerminate()
  }

  override def onLowMemory() {
    super.onLowMemory()

    GtApplication.onLowMemory ! null
  }

  override def onTrimMemory(level: Int) {
    super.onTrimMemory(level)

    GtApplication.onTrimMemory ! level
  }
}
