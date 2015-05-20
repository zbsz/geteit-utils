package com.geteit.app

import android.app.{Activity, Service}
import android.content.res.Configuration
import android.content.{Context, Intent}
import android.os.Bundle
import android.util.SparseArray
import android.view.ViewConfiguration
import com.geteit.events._
import com.geteit.util.Log._

object GtContext {
  private implicit val tag: LogTag = "GtContext"

  val Global: GtContext = GtApplication.APP_INSTANCE

  object globals {
    implicit val context = GtContext.Global
    implicit val eventContext = EventContext.Global
  }

  val onContextResumed = new Publisher[GtContext]
  val onContextDestroyed = new Publisher[GtContext]


  private lazy val configs = {
    try {
      val f = classOf[ViewConfiguration].getDeclaredField("sConfigurations")
      f.setAccessible(true)
      f.get(null).asInstanceOf[SparseArray[ViewConfiguration]]
    } catch {
      case e: Throwable =>
        error("ViewConfiguration loading failed", e)
        new SparseArray[ViewConfiguration]()
    }
  }

  private lazy val viewConfigContextField = {
    try {
      val f = classOf[ViewConfiguration].getDeclaredField("mContext")
      f.setAccessible(true)
      Some(f)
    } catch {
      case e: Throwable =>
        debug(s"viewConfigContextField failed: ${e.getMessage}")
        None
    }
  }

  private lazy val browserFrameCallback = {
    try {
      val f = Class.forName("android.webkit.BrowserFrame").getDeclaredField("sConfigCallback")
      f.setAccessible(true)
      Some(f)
    } catch {
      case e: Throwable =>
        debug(s"browserFrameCallback failed: ${e.getMessage}")
        None
    }
  }

  onContextDestroyed { ctx: GtContext =>
    val density = (100.0f * ctx.getResources.getDisplayMetrics.density).toInt
    val vc = configs.get(density)
    if (vc != null) {
      try {
        viewConfigContextField foreach (_.get(vc) match {
          case c: GtContext if c.ctxDestroyed.currentValue => configs.remove(density)
          case _ => //ignore
        })
      } catch {
        case e: Throwable => error("onContextDestroyed failed", e)
      }
    }

    try {
      browserFrameCallback foreach (_.set(null, null))
    } catch {
      case e: Throwable => error("browserFrameCallback failed", e)
    }

  }(EventContext.Global)
}

trait GtContext extends Context {

  implicit val ctx = this
  implicit val eventContext = new EventContext {}

  val ctxCreate = new Signal[Boolean](false)
  val ctxDestroyed = new Signal[Boolean](false) with ForcedEventSource[Boolean]

  @deprecated
  val ctxDestroy = new Publisher[Null] with ForcedEventSource[Null]

  val ctxPause = new Publisher[Null] with ForcedEventSource[Null]
  val ctxResume = new Publisher[Null] with ForcedEventSource[Null]
  val ctxConfigChanged = new Publisher[Configuration] with ForcedEventSource[Configuration]
  val ctxOnActivityResult = new Publisher[(Int, Int, Intent)] with ForcedEventSource[(Int, Int, Intent)]


  protected def publishCreate() {
    ctxCreate ! true
  }

  protected def publishDestroy() {
    ctxDestroyed ! true
    ctxDestroy ! null
    GtContext.onContextDestroyed ! this

    ctxCreate.unsubscribeAll()
    ctxDestroyed.unsubscribeAll()
    ctxDestroy.unsubscribeAll()
    ctxPause.unsubscribeAll()
    ctxResume.unsubscribeAll()
    ctxConfigChanged.unsubscribeAll()
    ctxOnActivityResult.unsubscribeAll()
    eventContext.onContextDestroy()
  }

  protected def publishConfigChange(c: Configuration) {
    ctxConfigChanged ! c
  }

  protected def publishPause() {
    ctxPause ! null
    eventContext.onContextStop()
  }

  protected def publishResume() {
    eventContext.onContextStart()
    ctxResume ! null
    GtContext.onContextResumed ! this
  }
}

trait GtActivityContext extends Activity with GtContext {

  protected override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    publishCreate()
  }

  protected override def onResume() {
    super.onResume()
    publishResume()
  }

  protected override def onPause() {
    super.onPause()
    publishPause()
  }


  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent) {
    ctxOnActivityResult !(requestCode, resultCode, data)

    super.onActivityResult(requestCode, resultCode, data)
  }

  override def onConfigurationChanged(newConfig: Configuration) {
    super.onConfigurationChanged(newConfig)
    publishConfigChange(newConfig)
  }

  protected override def onDestroy() {
    super.onDestroy()
    publishDestroy()
  }
}

trait GtServiceContext extends Service with GtContext {

  def context = getApplicationContext

  override def onCreate() {
    super.onCreate()
    publishCreate()
    eventContext.onContextStart()
  }

  override def onDestroy() {
    super.onDestroy()
    eventContext.onContextStop()
    publishDestroy()
  }
}
