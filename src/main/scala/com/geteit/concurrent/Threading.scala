package com.geteit.concurrent

import android.os.{Looper, Handler}
import com.geteit.util.Log

import scala.concurrent.ExecutionContext

object Threading {
  implicit val global: ExecutionContext = LimitedExecutionContext.CpuBoundExecutor

  implicit val io: ExecutionContext = LimitedExecutionContext.CpuBoundExecutor

  implicit val ui: ExecutionContext = new ExecutionContext {
    val handler = new Handler(Looper.getMainLooper)
    override def reportFailure(cause: Throwable): Unit = Log.error("failure reported", cause)("Threading.ui")
    override def execute(runnable: Runnable): Unit = handler.post(runnable)
  }
}
