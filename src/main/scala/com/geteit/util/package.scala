package com.geteit

package object util {

  // kestrel operator
  def returning[A](value: A)(body: A => Unit) = {
    body(value)
    value
  }
}
