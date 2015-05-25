package com.geteit.inject

import android.app.Activity
import android.content.{ContentResolver, Context}
import com.geteit.app.{GtApplication, GtContext}
import com.geteit.events.EventContext
import com.geteit.util.AtomicUpdateMap
import com.geteit.util.GtAssert


object Injectable {
  private val instances = new AtomicUpdateMap[Context, AtomicUpdateMap[Manifest[_], Any]]

  private def instance[T: Manifest](ctx: GtContext, f: => T): T = {
    GtAssert(!ctx.ctxDestroyed, "accessing instance in destroyed context")

    instances.getOrElseUpdate(ctx, new AtomicUpdateMap[Manifest[_], Any]).getOrElseUpdate(manifest, f).asInstanceOf[T]
  }

  import GtContext.globals._
  GtContext.onContextDestroyed { instances.remove(_) }

  implicit val appFactory = Factory[android.app.Application](GtApplication.APP_INSTANCE)
  implicit val activityFactory = new Factory[Activity](_.asInstanceOf[Activity])
  implicit val contentResolverFactory = new Factory[ContentResolver](_.getContentResolver)
}

object Factory {
  def apply[T](f: => T) = new Factory(_ => f)
}

class Factory[T](f: GtContext => T) {
  def apply(ctx: GtContext) = f(ctx)
}

trait Injectable {
  import Injectable._

  def inject[T](implicit ctx: GtContext, m: Manifest[T], factory: Factory[T]): T = inject[T](factory(ctx))(ctx, m)

  def inject[T](factory: => T)(implicit ctx: GtContext, m: Manifest[T]): T = {
    if (classOf[GtSingleton].isAssignableFrom(m.runtimeClass)) singleton[T](factory)
    else if (classOf[GtContextSingleton].isAssignableFrom(m.runtimeClass)) contextSingleton(factory)
    else factory
  }

  def contextSingleton[T](factory: => T)(implicit ctx: GtContext, m: Manifest[T]) = instance[T](ctx, factory)

  def singleton[T: Manifest](factory: => T) = instance[T](GtContext.Global, factory)
}

trait GtSingleton {
  implicit val context = GtContext.Global
  implicit val eventContext = EventContext.Global
}

trait GtContextSingleton {
  implicit val context: GtContext
  implicit val eventContext = context.eventContext
}
