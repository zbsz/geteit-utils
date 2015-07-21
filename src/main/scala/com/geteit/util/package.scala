package com.geteit

import android.app.Activity
import android.content.{Context, Intent}
import android.net.Uri
import android.support.v4.app.FragmentActivity
import android.view.View
import android.view.View.OnClickListener
import com.geteit.concurrent.Threading
import com.geteit.util.Log._

import scala.concurrent.{ExecutionContext, Future}
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

  implicit def func_to_ViewOnClickListener[A](f: View => A): OnClickListener = new View.OnClickListener() {
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

  implicit class RichFuture[A](base: Future[A]) {
    def zip[B](f: Future[B])(implicit executor: ExecutionContext) = RichFuture.zip(base, f)
    def recoverWithLog(reportHockey: Boolean = false)(implicit tag: LogTag) = RichFuture.recoverWithLog(base, reportHockey)
  }

  object RichFuture {

    def zip[A, B](f1: Future[A], f2: Future[B])(implicit executor: ExecutionContext): Future[(A, B)] =
      for(r1 <- f1; r2 <- f2) yield (r1, r2)


    def traverseSequential[A, B](in: Seq[A])(f: A => Future[B])(implicit executor: ExecutionContext): Future[Seq[B]] = {
      def processNext(remaining: Seq[A], acc: List[B] = Nil): Future[Seq[B]] =
        if (remaining.isEmpty) Future.successful(acc.reverse)
        else f(remaining.head) flatMap { res => processNext(remaining.tail, res :: acc) }

      processNext(in)
    }

    def recoverWithLog[A](f: Future[A], uploadReport: Boolean = false)(implicit tag: LogTag) = f.recover(LoggedTry.errorHandler(uploadReport))(Threading.global)

    /**
     * Process sequentially and ignore results.
     * Similar to traverseSequential, but doesn't care about the result, and continues on failure.
     */
    def processSequential[A, B](in: Seq[A])(f: A => Future[B])(implicit executor: ExecutionContext, tag: LogTag): Future[Unit] = {
      def processNext(remaining: Seq[A]): Future[Unit] =
        if (remaining.isEmpty) Future.successful(())
        else LoggedTry(recoverWithLog(f(remaining.head), uploadReport = true)).getOrElse(Future.successful(())) flatMap { _ => processNext(remaining.tail) }

      processNext(in)
    }
  }
}
