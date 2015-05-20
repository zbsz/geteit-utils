package com.geteit.util

import scala.collection.Iterator
import scala.collection.concurrent.{Map, TrieMap}

object AtomicUpdateMap {

  private class Lazy[T](value: => T) extends Proxy {
    def self = value // for proxy
    // Need to be careful with Proxy; any call to toString,
    // hashCode, or equals will force our call-by-name value.
    // I *think* only the oldvalue/newvalue form of replace() could do this.

    /** Get the value out of the Lazy.  Value will be evaluated exactly once. */
    lazy val get: T = value
  }

}

/**
 * Concept:
 *
 * Wrap incoming map values in a lazy wrapper so we can control when/if they are evaluated.  Make sure the
 * lazy wrapper can evaluate its contents at most once.  Maintain a private j.u.c.ConcurrentHashMap of key->Lazy(value),
 * and delegate everything possible to that map, translating from Lazy[V] -> V as required.
 * This allows us to implement getOrElseUpdate in terms of putIfAbsent, which in turn allows us to only evaluate our
 * value if we "win" the putIfAbsent race.
 *
 * Pros:
 * - leverages j.u.c.ConcurrentHashMap with no changes
 * - works with any implementation of j.u.c.ConcurrentMap
 *
 * Downsides:
 * - unmeasured overhead from wrapping values in Lazy[T], resolving the call-by-names, lazy vals; might be terrible
 * - possibly confusing layers of call-by-names and lazy val
 */
class AtomicUpdateMap[A, B] extends Map[A, B] {

  import AtomicUpdateMap._

  private val _underlying = new TrieMap[A, Lazy[B]]

  def +=(kv: (A, B)): this.type = {
    val (k, v) = kv
    _underlying += ((k, new Lazy(v)))
    this
  }

  def -=(key: A): this.type = {
    _underlying -= key
    this
  }

  // Delegate to an iterator on the underlying map, but translate Lazy[B]s => Bs
  def iterator: Iterator[(A, B)] =
    new Iterator[(A, B)] {
      private val _underlyingIterator = _underlying.iterator

      def next(): (A, B) = {
        val (k, v) = _underlyingIterator.next()
        (k, v.get)
      }

      def hasNext: Boolean = _underlyingIterator.hasNext
    }

  def get(key: A): Option[B] = _underlying.get(key).map { _.get }

  def replace(k: A, v: B): Option[B] = _underlying.replace(k, new Lazy(v)).map { _.get }

  def replace(k: A, oldvalue: B, newvalue: B): Boolean = _underlying.replace(k, new Lazy(oldvalue), new Lazy(newvalue))

  def remove(k: A, v: B): Boolean = _underlying.remove(k, new Lazy(v))

  def putIfAbsent(k: A, v: B): Option[B] = _underlying.putIfAbsent(k, new Lazy(v)).map { _.get }

  // see class comment above
  override def getOrElseUpdate(key: A, op: => B): B = {
    // careful to not force lazyOp until we know we were the first to put key
    val lazyOp = new Lazy(op)
    _underlying.putIfAbsent(key, lazyOp) match {
      case Some(oldval) =>
        // key was already in the map with a value, don't evaluate the new lazyOp we created; return existing value
        oldval.get
      case _ =>
        // no previous value for key; return that value in our Lazy; evaluation finally occurs here
        lazyOp.get
    }
  }
}
