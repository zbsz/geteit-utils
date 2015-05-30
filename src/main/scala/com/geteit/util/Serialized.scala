package com.geteit.util

import com.geteit.concurrent.{CancellableFuture, LimitedExecutionContext}

import scala.collection.mutable

object Serialized {
  private implicit val dispatcher = new LimitedExecutionContext()

  private val locks = new mutable.HashMap[Any, CancellableFuture[_]]

  def apply[A](key: Any*)(body: => CancellableFuture[A]): CancellableFuture[A] = CancellableFuture {
    val future = locks.get(key).fold(body) { lock =>
      lock.recover { case _ => } flatMap(_ => body)
    }
    locks += (key -> future)
    future.onComplete {
      case _ => if (locks.get(key).contains(future)) locks -= key
    }
    future
  }.flatten
}

