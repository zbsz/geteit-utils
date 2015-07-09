package com.geteit.inject

import com.geteit.inject.Module.Bindings
import com.geteit.util.returning

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

trait Injector {
  def binding[T: Manifest]: Option[() => T]
}

class Module(val factory: Injector => Bindings, val parent: Option[Injector] = None) extends Injector with Injectable {
  private lazy val bindings = factory(this)

  override def binding[T: Manifest] = bindings.get(implicitly[Manifest[T]]).asInstanceOf[Option[() => T]].orElse(parent.flatMap(_.binding[T]))

  def ::(m: Module) = new Module({ inj => factory(inj) ++ m.factory(inj) })
}

class ImmutableWrapper(module: Module) extends Injector {
  override def binding[T: Predef.Manifest]: Option[() => T] = module.binding[T]

  def ::(m: Module) = new Module(m.factory, Some(this))
}

object ImmutableWrapper {
  def apply(m: Module) = new ImmutableWrapper(m)
}

object Module {
  type Bindings = Map[Manifest[_], () => _]

  class Builder private[inject] () extends Injector with Injectable {
    var injector: Injector = _
    private val bindings = Map.newBuilder[Manifest[_], () => _]

    protected case class Binding[T](cls: Manifest[T]) {
      def to(fn: => T) = bindings += cls -> Singleton(() => fn)
      def toProvider(fn: => T) = bindings += cls -> Provider(() => fn)
    }

    def bind[T: Manifest] = new Binding[T](implicitly[Manifest[T]])
    def apply[T: Manifest] = new Binding[T](implicitly[Manifest[T]])

    override def binding[T: Predef.Manifest]: Option[() => T] = injector.binding[T]


    private[inject] def result(inj: Injector): Bindings = {
      this.injector = inj
      bindings.result()
    }
  }

  def apply(f: Builder => Unit): Module = new Module(returning(new Builder)(f).result)
}

