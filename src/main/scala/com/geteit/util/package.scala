package com.geteit

import android.app.Activity
import android.content.{Context, Intent}
import android.net.Uri
import android.support.v4.app.FragmentActivity
import android.view.View
import android.view.View.OnClickListener

import scala.language.implicitConversions

package object util {

  // kestrel operator
  def returning[A](value: A)(body: A => Unit) = { body(value); value }

  /**
   * Calculates density accounting for screen size, on large screen we want the UI to be scalled up (pretend to have higher density).
   */
  def scaledDensity(implicit context: Context) = {
    val dm = context.getResources.getDisplayMetrics
    val density: Float = dm.density
    val dpSize = Math.min(dm.widthPixels / density, dm.heightPixels / density)
    if (dpSize > 480) density * math.ceil(dpSize / 480 * 4).toInt / 4
    else density
  }

  /** returns number of pixels from given amount of display pixels **/
  def dp(amount: Float)(implicit context: Context) = (amount * context.getResources.getDisplayMetrics.density + .5f).toInt

  def sdp(v: Float)(implicit context: Context) = (scaledDensity * v).toInt

  implicit class Dimension(amount: Float) {
    def dp(implicit context: Context) = com.geteit.util.dp(amount)
    def sdp(implicit context: Context) = com.geteit.util.sdp(amount)
  }

  def visibility(visible: Boolean) = if (visible) View.VISIBLE else View.GONE

  def clamp(v:Float, min:Float, max:Float) = math.min(max, math.max(v, min))
  def clamp(v:Int, min:Int, max:Int) = math.min(max, math.max(v, min))

  def intent[T](implicit context: Context, m: scala.reflect.Manifest[T]) = new Intent(context, m.runtimeClass)
  def intent[T](data: Uri)(implicit context: Context, m: scala.reflect.Manifest[T]) = new Intent(context, m.runtimeClass).setData(data)
  def systemService[T](name: String)(implicit context: Context) = context.getSystemService(name).asInstanceOf[T]

  def startActivity[T](implicit context: Context, m: scala.reflect.Manifest[T]) {
    context.asInstanceOf[Activity].startActivity(intent[T])
  }
  def startService[T](implicit context: Context, m: scala.reflect.Manifest[T]) = context.startService(intent[T])
  def startService[T](data: Uri)(implicit context: Context, m: scala.reflect.Manifest[T]) = context.startService(intent[T](data))

  implicit def fragmentManager(implicit context: Context) = context.asInstanceOf[FragmentActivity].getSupportFragmentManager
  implicit def loaderManager(implicit context: Context) = context.asInstanceOf[FragmentActivity].getSupportLoaderManager

  implicit def int_to_dimension(v: Int): Dimension = new Dimension(v)

  implicit def boolean_to_visibility(visible: Boolean): Int = visibility(visible)

  implicit def lazy2ViewOnClickListener(f: => Any): OnClickListener with Object {def onClick(view: View): Unit} = new View.OnClickListener() {
    def onClick(view: View) {
      f
    }
  }

  implicit def func_to_ViewOnClickListener(f: View => Unit): OnClickListener with Object {def onClick(view: View): Unit} = new View.OnClickListener() {
    def onClick(view: View) {
      f(view)
    }
  }

  implicit def func2_to_runnable(f: () => Unit): Runnable with Object {def run(): Unit} = new Runnable() {
    override def run() {
      f()
    }
  }

  implicit def lazy_to_runnable(f: => Unit): Runnable with Object {def run(): Unit} = new Runnable() {
    override def run() {
      f
    }
  }
}
