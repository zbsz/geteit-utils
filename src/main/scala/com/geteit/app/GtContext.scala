package com.geteit.app

import android.app.{Activity, Service}
import android.content.res.Configuration
import android.content.{Context, ContextWrapper, Intent}
import android.os.Bundle
import android.preference.PreferenceManager
import android.support.v4.app.{Fragment, FragmentActivity}
import android.util.SparseArray
import android.view.View.OnClickListener
import android.view.{View, ViewConfiguration, ViewStub}
import com.geteit.events._
import com.geteit.inject._
import com.geteit.util.Log._

import scala.language.implicitConversions

object GtContext {
  private implicit val tag: LogTag = "GtContext"

  var Global: GtContext = null

  object globals {
    implicit val context = GtContext.Global
    implicit val eventContext = EventContext.Global
  }

  val onContextResumed = new Publisher[GtContext]
  val onContextDestroyed = new Publisher[GtContext]

  implicit def apply(context: Context): GtContext = context match {
    case ctx: GtContext => ctx
    case wrapper: ContextWrapper => apply(wrapper.getBaseContext)
    case _ => throw new IllegalArgumentException("Expecting GtContext, got: " + context)
  }

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
          case c: GtContext if c.ctxDestroyed.currentValue.contains(true) => configs.remove(density)
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

trait GtContext extends Context with Injector {

  implicit val ctx = this
  implicit val eventContext = new EventContext {}

  val ctxCreate = new Signal[Boolean]
  val ctxDestroyed = new Signal[Boolean] with ForcedEventSource[Boolean]
  ctxDestroyed ! false

  val ctxPause = new Publisher[Null] with ForcedEventSource[Null]
  val ctxResume = new Publisher[Null] with ForcedEventSource[Null]
  val ctxConfigChanged = new Publisher[Configuration] with ForcedEventSource[Configuration]
  val ctxOnActivityResult = new Publisher[(Int, Int, Intent)] with ForcedEventSource[(Int, Int, Intent)]

  lazy val module: Module = {
    verbose(s"context: $this, create module")("GtContext")
    this match {
      case app: GtApplication => GtModule
      case c =>
        verbose(s"other")("GtContext")
        GtApplication.APP_INSTANCE.contextModule(c) :: ImmutableWrapper(getApplicationContext.asInstanceOf[GtApplication].module)
    }
  }

  override def binding[T: Manifest] = module.binding[T]

  protected def publishCreate() {
    ctxCreate ! true
  }

  protected def publishDestroy() {
    ctxDestroyed ! true
    GtContext.onContextDestroyed ! this

    ctxCreate.unsubscribeAll()
    ctxDestroyed.unsubscribeAll()
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

trait ViewFinder {
  def findById[V <: View](id: Int) : V
  def stub[V <: View](id: Int) : V = findById[ViewStub](id).inflate().asInstanceOf[V]

  implicit def id_to_view[V <: View](id: Int) : V = findById[V](id)
}

trait ViewHelper extends View with ViewFinder with Injectable with ViewEventContext {
  lazy implicit val con: GtContext = getContext

  def findById[V <: View](id: Int) = findViewById(id).asInstanceOf[V]
  def visible = getVisibility == View.VISIBLE
  def onClick(f: => Any) { setOnClickListener(new OnClickListener { def onClick(v: View) { f }}) }
}

trait ServiceHelper extends Service with Injectable with GtServiceContext {

}

trait FragmentHelper extends Fragment with ViewFinder with Injectable with FragmentEventContext {
  lazy implicit val context = getActivity.asInstanceOf[GtActivityContext]
  lazy val defaultPreferences = PreferenceManager.getDefaultSharedPreferences(context)
  implicit def holder_to_view[T <: View](h: ViewHolder[T]): T = h.get
  private var views: List[ViewHolder[_]] = Nil

  def findById[V <: View](id: Int) = {
    val res = getView.findViewById(id)
    if (res != null) res.asInstanceOf[V]
    else getActivity.findViewById(id).asInstanceOf[V]
  }
  def view[V <: View](id: Int) = {
    val h = new ViewHolder[V](id, this)
    views ::= h
    h
  }

  override def onDestroyView() {
    super.onDestroyView()
    views foreach(_.clear())
  }
}

trait ActivityHelper extends Activity with ViewFinder with Injectable with GtActivityContext {
  lazy implicit val context: GtContext = this
  lazy val defaultPreferences = PreferenceManager.getDefaultSharedPreferences(context)

  def findById[V <: View](id: Int) = findViewById(id).asInstanceOf[V]

  def findFragment[T](id: Int) : T = {
    this.asInstanceOf[FragmentActivity].getSupportFragmentManager.findFragmentById(id).asInstanceOf[T]
  }
}

class ViewHolder[T <: View](id: Int, finder: ViewFinder) {
  var view: T = null.asInstanceOf[T]

  def get: T = {
    if (view == null) view = finder.findById(id)
    view
  }

  def clear() {
    view = null.asInstanceOf[T]
  }
}
