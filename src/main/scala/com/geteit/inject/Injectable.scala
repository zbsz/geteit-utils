package com.geteit.inject

import scala.Predef
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

  def ::(inj: Injector) = new Injector {
    override def binding[T: Predef.Manifest]: Option[() => T] = inj.binding.orElse(self.binding)
  }
}

class ImmutableWrapper(module: Module) extends Injector {
  override def binding[T: Predef.Manifest]: Option[() => T] = module.binding[T]
}

object ImmutableWrapper {
  def apply(m: Module) = new ImmutableWrapper(m)
}

class Module extends Injector with Injectable {
  protected implicit val inj: Injector = this

  private[inject] var parent = Option.empty[Injector]
  private[inject] val bindings = new scala.collection.mutable.HashMap[Manifest[_], () => _]

  protected class Binding[T](cls: Manifest[T]) {
    def to(fn: => T) = bindings += cls -> Singleton(() => fn)
    def toProvider(fn: => T) = bindings += cls -> Provider(() => fn)
  }

  protected def bind[T: Manifest] = new Binding[T](implicitly[Manifest[T]])

  override def binding[T: Predef.Manifest]: Option[() => T] =
    internal(implicitly[Manifest[T]]).orElse(parent.flatMap(_.binding[T]))

  private[inject] def internal[T](m: Manifest[T]) = bindings.get(m).asInstanceOf[Option[() => T]]

  def ::(m: Module) = new ModuleChain(Seq(m, this))
}

class ModuleChain(modules: Seq[Module]) extends Injector {
  modules.foreach(_.parent = Some(this))

  private val cache = new scala.collection.mutable.HashMap[Manifest[_], Option[() => _]]

  private def internal[T](m: Manifest[T]) =
    modules.iterator.map(_.internal(m)).collectFirst { case Some(f) => f }

  override def binding[T: Predef.Manifest] = {
    val m = implicitly[Manifest[T]]
    cache.getOrElseUpdate(m, internal(m)).asInstanceOf[Option[() => T]]
  }

  def ::(m: Module) = new ModuleChain(m +: modules)
}
