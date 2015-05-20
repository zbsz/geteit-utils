package com.geteit.util

object GtAssert {
  def apply(cond: => Boolean, msg: => String = "") = if (!cond) throw new AssertionError(msg)
}
