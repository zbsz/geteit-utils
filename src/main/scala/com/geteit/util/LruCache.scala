package com.geteit.util

import android.content.ComponentCallbacks2
import com.geteit.app.GtApplication
import com.geteit.events.EventContext
import Log._

class LruCache[K, V](maxSize: Int)(implicit eventContext: EventContext) extends android.support.v4.util.LruCache[K, V](maxSize) {
  import LruCache._

  GtApplication.onLowMemory { _ => evictAll() }

  GtApplication.onTrimMemory { level: Int =>
    TrimFactors.collectFirst { case (l, factor) if l >= level =>
      val trimmedSize = (factor * maxSize()).toInt
      verbose(s"onTrimMemory($level) - trimToSize: $trimmedSize")
      trimToSize(trimmedSize)
      System.gc()
    }
  }

  def update(k: K, v: V) {
    put(k, v)
  }

  def apply(k: K): Option[V] = Option(get(k))

  def apply(k: K, f: => Option[V]): Option[V] = Option(get(k)).orElse {
    f match {
      case Some(v) => update(k, v); Some(v)
      case None => None
    }
  }
}

object LruCache {
  private implicit val tag: LogTag = "LruCache"

  val TrimFactors = Seq(
    ComponentCallbacks2.TRIM_MEMORY_RUNNING_LOW -> .75f,
    ComponentCallbacks2.TRIM_MEMORY_RUNNING_CRITICAL -> .5f,
    ComponentCallbacks2.TRIM_MEMORY_UI_HIDDEN -> .25f,
    ComponentCallbacks2.TRIM_MEMORY_MODERATE -> 0f
  )
}
