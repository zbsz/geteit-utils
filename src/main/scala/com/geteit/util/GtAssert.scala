package com.geteit.util

object GtAssert {
  def apply(cond: => Boolean, msg: => String = "") = if (!cond) throw new AssertionError(msg)

  def assertUIThread() = apply(Thread.currentThread.getName.startsWith("main"), s"wrong thread: ${Thread.currentThread.getName}")
}
