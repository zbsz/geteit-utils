package com.geteit.util

import scala.reflect.ClassTag

object Log {
  type LogTag = String

  @volatile var minimumLogLevel: Int = android.util.Log.VERBOSE

  def logTagFor[A <: Singleton: ClassTag](a: A): String =  logTagFor[A]
  def logTagFor[A: ClassTag]: String = implicitly[ClassTag[A]].runtimeClass.getSimpleName

  def error(message: String, cause: Throwable)(implicit tag: LogTag): Unit = android.util.Log.e(tag, message, cause)
  def error(message: String)(implicit tag: LogTag): Unit = android.util.Log.e(tag, message)
  def warn(message: String, cause: Throwable)(implicit tag: LogTag): Unit = android.util.Log.w(tag, message, cause)
  def warn(message: String)(implicit tag: LogTag): Unit = android.util.Log.w(tag, message)
  def info(message: => String)(implicit tag: LogTag): Unit = android.util.Log.i(tag, message)
  def debug(message: => String)(implicit tag: LogTag): Unit = android.util.Log.d(tag, message)
  def verbose(message: => String)(implicit tag: LogTag): Unit = android.util.Log.v(tag, message)
  def logTime[A](message: => String)(body: => A)(implicit tag: LogTag): A = {
    val time = System.nanoTime
    try {
      body
    } finally {
      android.util.Log.v(tag, message + ": " + ((System.nanoTime - time) / 1000 / 1000f) + " ms")
    }
  }
}
