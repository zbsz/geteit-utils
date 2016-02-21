package com.geteit.inject

import scala.reflect.Manifest

trait Injectable {
  def inject[T: Manifest](implicit inj: Injector) =
    inj.binding[T].getOrElse(throw new Exception(s"No binding for: ${implicitly[Manifest[T]].runtimeClass.getName} in $inj")).apply()
}

case class Provider[T](fn: () => T) extends (() => T) {
  def apply() = fn()
}
case class Singleton[T](fn: () => T) extends (() => T) {
  lazy val value = fn()
  def apply() = value
}

trait Injector { self =>
  def binding[T: Manifest]: Option[() => T]

  private[inject] var parent = Option.empty[Injector]

  def ::(inj: Injector) = new Injector {
    inj.parent = Some(self)

    override def binding[T: Predef.Manifest]: Option[() => T] = inj.binding[T].orElse(parent.flatMap(_.binding[T]))

    override def toString: String = s"Injector($inj :: $self)"
  }
}

class Module extends Injector with Injectable {
  protected implicit val inj: Injector = this

  private[inject] val bindings = new scala.collection.mutable.HashMap[Manifest[_], () => _]

  protected class Binding[T](cls: Manifest[T]) {
    def to(fn: => T) = bindings += cls -> Singleton(() => fn)
    def toProvider(fn: => T) = bindings += cls -> Provider(() => fn)
  }

  protected def bind[T: Manifest] = new Binding[T](implicitly[Manifest[T]])

  override def binding[T: Predef.Manifest]: Option[() => T] =
    internal(implicitly[Manifest[T]]).orElse(parent.flatMap(_.binding[T]))

  private[inject] def internal[T](m: Manifest[T]) = bindings.get(m).asInstanceOf[Option[() => T]]

  override def toString: String = s"Module(bindings: $bindings, parent: $parent)"
}
