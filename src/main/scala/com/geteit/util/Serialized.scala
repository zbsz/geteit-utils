package com.geteit.util

import com.geteit.concurrent.{CancellableFuture, LimitedExecutionContext}

import scala.collection.mutable
import scala.concurrent.Future

object Serialized {
  private implicit val dispatcher = new LimitedExecutionContext()

  private val locks = new mutable.HashMap[Any, Future[_]]

  def apply[A](key: Any*)(body: => CancellableFuture[A]): CancellableFuture[A] = CancellableFuture {
    val future = locks.get(key).fold(body) { lock =>
      CancellableFuture.lift(lock.recover { case _ => }) flatMap(_ => body)
    }
    locks += (key -> future.future)
    future.onComplete {
      case _ => if (locks.get(key).contains(future)) locks -= key
    }
    future
  }.flatten


  def future[A](key: Any*)(body: => Future[A]): Future[A] = Future {
    val future = locks.get(key).fold(body) { lock =>
      lock.recover { case _ => } flatMap(_ => body)
    }
    locks += (key -> future)
    future.onComplete {
      case _ => if (locks.get(key).contains(future)) locks -= key
    }
    future
  }.flatMap(identity)
}

