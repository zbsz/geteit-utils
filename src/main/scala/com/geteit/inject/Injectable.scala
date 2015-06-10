package com.geteit.inject

import android.app.{ActivityManager, Activity}
import android.content.{ContentResolver, Context}
import com.geteit.app.{GtApplication, GtContext}
import com.geteit.events.EventContext
import com.geteit.util.AtomicUpdateMap
import com.geteit.util.GtAssert
import com.geteit.util.returning

trait Module {
  def apply[T: Manifest]: Option[Factory[T]]
}

object Module {
  class Builder {
    private var factories = Map.empty[Manifest[_], Factory[_]]

    def +=[T: Manifest](f: Factory[T]) = {
      factories += implicitly[Manifest[T]] -> f
      this
    }

    def result = new Module {
      override def apply[T: Manifest]: Option[Factory[T]] = factories.get(implicitly[Manifest[T]]).map(_.asInstanceOf[Factory[T]])
    }
  }

  def apply(f: Builder => Unit) = returning(new Builder)(f).result
}


object Injectable {
  private var modules = List.empty[Module]
  private val instances = new AtomicUpdateMap[Context, AtomicUpdateMap[Manifest[_], Any]]

  private def instance[T: Manifest](ctx: GtContext, f: => T): T = {
    GtAssert(!ctx.ctxDestroyed, "accessing instance in destroyed context")

    instances.getOrElseUpdate(ctx, new AtomicUpdateMap[Manifest[_], Any]).getOrElseUpdate(manifest, f).asInstanceOf[T]
  }

  def registerModule(m: Module) = modules ::= m

  import GtContext.globals._
  GtContext.onContextDestroyed { instances.remove(_) }
}

object Factory {

  def apply[T](f: => T) = new Factory(_ => f)

  implicit val appFactory = Factory[android.app.Application](GtApplication.APP_INSTANCE)
  implicit val activityFactory = new Factory[Activity](_.asInstanceOf[Activity])
  implicit val contentResolverFactory = new Factory[ContentResolver](_.getContentResolver)
  implicit val activityManagerFactory = new Factory[ActivityManager](_.getSystemService(Context.ACTIVITY_SERVICE).asInstanceOf[ActivityManager])
}

class Factory[T](f: GtContext => T) {
  def apply(ctx: GtContext) = f(ctx)
}

trait Injectable {
  import Injectable._

  def inject[T](implicit ctx: GtContext, m: Manifest[T], factory: Factory[T]): T = {
    def f = modules.view.map(_.apply[T]).collect { case Some(f) => f }.headOption.getOrElse(factory)
    inject[T](f(ctx))(ctx, m)
  }

  def inject[T](factory: => T)(implicit ctx: GtContext, m: Manifest[T]): T = {
    if (classOf[GtSingleton].isAssignableFrom(m.runtimeClass)) instance[T](GtContext.Global, factory)
    else if (classOf[GtContextSingleton].isAssignableFrom(m.runtimeClass)) instance[T](ctx, factory)
    else factory
  }
}

trait GtSingleton {
  implicit val context = GtContext.Global
  implicit val eventContext = EventContext.Global
}

trait GtContextSingleton {
  implicit val context: GtContext
  implicit val eventContext = context.eventContext
}
