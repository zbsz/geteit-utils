package com.geteit.util

import com.geteit.concurrent.CancellableFuture.CancelException
import com.geteit.util.Log._

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object LoggedTry {
  def apply[A](f: => A)(implicit tag: LogTag): Try[A] = {
    try {
      Success(f)
    } catch {
      errorHandler(uploadReport = true)
    }
  }

  def errorHandler[A](uploadReport: Boolean = false)(implicit tag: LogTag): PartialFunction[Throwable, Try[A]] = {
    case CancelException => Failure(CancelException)
    case e: BoxedError => Failure(e) // already processed, no need to log anything
    case NonFatal(e) =>
      warn("logged try failed", e)
      Failure(e)
    case e: Throwable =>
      // TODO: upload report
      error("logged try got fatal error", e)
      Failure(BoxedError(e))
  }
}

case class BoxedError(cause: Throwable) extends RuntimeException("BoxedError", cause)

object BoxedError {

  def boxFatal[A](body: => A) = try {
    body
  } catch {
    case NonFatal(e) => throw e
    case e: Throwable => throw new BoxedError(e)
  }

  def boxOoM[A](body: => A) = try {
    body
  } catch {
    case e: OutOfMemoryError => throw new BoxedError(e)
  }
}
