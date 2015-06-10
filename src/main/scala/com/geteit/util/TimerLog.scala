package com.geteit.util

import Log._

object TimerLog {
  val VERBOSE = false
}

trait TimerLog {

  private var time: Long = System.currentTimeMillis()
  private var marks: List[(Long, String)] = Nil

  def mark(msg: String) {
    if (TimerLog.VERBOSE) {
      val t = System.currentTimeMillis()
      marks ::=(t - time, msg)
      time = t
    }
  }

  def log(header: String = null)(implicit tag: LogTag) {
    if (TimerLog.VERBOSE) verbose(header + "\n" + marks.reverse.map(p => "%5d - %s".format(p._1, p._2)).mkString("\n"))
  }
}
