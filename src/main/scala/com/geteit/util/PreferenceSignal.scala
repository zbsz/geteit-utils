package com.geteit.util

import android.content.SharedPreferences.{Editor, OnSharedPreferenceChangeListener}
import android.content.{Context, SharedPreferences}
import android.preference.PreferenceManager
import com.geteit.concurrent.{LimitedExecutionContext, Threading}
import com.geteit.events._
import com.geteit.util.PreferenceSignal.Helper

import scala.concurrent.Future
import scala.concurrent.duration._

object PreferenceSignal {

  trait Helper[T] {
    def read(prefs: SharedPreferences, key: String, default: T): T
    def write(editor: SharedPreferences.Editor, key: String, value: T)
  }

  implicit val LongHelper = new Helper[Long] {
    def read(prefs: SharedPreferences, key: String, default: Long = 0L): Long = prefs.getLong(key, default)
    def write(editor: Editor, key: String, value: Long) { editor.putLong(key, value) }
  }

  implicit val StringHelper = new Helper[String] {
    def read(prefs: SharedPreferences, key: String, default: String = ""): String = prefs.getString(key, default)
    def write(editor: Editor, key: String, value: String) { editor.putString(key, value) }
  }

  implicit val IntHelper = new Helper[Int] {
    def read(prefs: SharedPreferences, key: String, default: Int = 0): Int = prefs.getInt(key, default)
    def write(editor: Editor, key: String, value: Int) { editor.putInt(key, value) }
  }

  implicit val BooleanHelper = new Helper[Boolean] {
    def read(prefs: SharedPreferences, key: String, default: Boolean = false): Boolean = prefs.getBoolean(key, default)
    def write(editor: Editor, key: String, value: Boolean) { editor.putBoolean(key, value) }
  }

  def apply[T](key: String, default: T)(implicit context: Context, helper: Helper[T]) = new PreferenceSignal[T](key, PreferenceManager.getDefaultSharedPreferences(context), default)

  private implicit val PreferenceExecutionContext = new LimitedExecutionContext(1, Threading.io)

  private val SaveQueue = new ThrottledProcessingQueue[PreferenceSignal[_]](250.millis, { signals => Future {
    signals.distinct.groupBy(_.prefs) foreach {
      case (prefs, ss) =>
        val editor = prefs.edit()
        ss foreach { _.writeCurrent(editor) }
        editor.commit()
    }
  }})
}

class PreferenceSignal[T](private val key: String, preferences: => SharedPreferences, private val default: T)(implicit val helper: Helper[T]) extends Signal[T] with UiEventSource {
  import PreferenceSignal._
  private lazy val prefs = preferences

  disableAutowiring()

  private val listener = new OnSharedPreferenceChangeListener {
    def onSharedPreferenceChanged(sharedPreferences: SharedPreferences, k: String) {
      if (key == k) Future {
        PreferenceSignal.super.publish(read)
      }
    }
  }

  Future {
    publish(read)
  }

  private def read = helper.read(prefs, key, default)

  private def writeCurrent(editor: SharedPreferences.Editor): Unit = helper.write(editor, key, currentValue.getOrElse(default))

  def update(f: T => T) = Future { publish(f(read)) }

  override def publish(event: T): Unit = {
    if (value != event) {
      super.publish(event)

      SaveQueue ! this
    }
  }

  override protected def onWire(): Unit = Future {
    prefs.registerOnSharedPreferenceChangeListener(listener)
    super.publish(read)
  }

  override protected def onUnwire() = Future {
    prefs.unregisterOnSharedPreferenceChangeListener(listener)
  }
}

