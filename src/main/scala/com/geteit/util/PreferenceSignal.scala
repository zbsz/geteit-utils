package com.geteit.util

import android.content.SharedPreferences.{Editor, OnSharedPreferenceChangeListener}
import android.content.{Context, SharedPreferences}
import android.preference.PreferenceManager
import com.geteit.app.GtApplication
import com.geteit.concurrent.{LimitedExecutionContext, Threading}
import com.geteit.events._
import com.geteit.util.Log._
import com.geteit.util._
import com.geteit.util.PreferenceSignal.Helper

import scala.concurrent.Future
import scala.concurrent.duration._

object PreferenceSignal {
  private implicit val Tag: LogTag = "PreferenceSignal"

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

  private val signals = new scala.collection.mutable.HashMap[String, PreferenceSignal[_]]

  private lazy val prefs = returning(PreferenceManager.getDefaultSharedPreferences(GtApplication.APP_INSTANCE)) { prefs =>
    prefs.registerOnSharedPreferenceChangeListener(new OnSharedPreferenceChangeListener {
      def onSharedPreferenceChanged(sharedPreferences: SharedPreferences, k: String) = signals.get(k) foreach (_.reload())
    })
  }

  def apply[T](key: String, default: T)(implicit helper: Helper[T]) =
    signals.getOrElseUpdate(key, new PreferenceSignal[T](key, prefs, default)).asInstanceOf[PreferenceSignal[T]]

  private implicit val PreferenceExecutionContext = new LimitedExecutionContext(1, Threading.io)

  private val SaveQueue = new ThrottledProcessingQueue[PreferenceSignal[_]](250.millis, { signals => Future {
    verbose(s"saving preference signal values: $signals")
    signals foreach { signal =>
      val editor = prefs.edit()
      signal.writeCurrent(editor)
      editor.commit()
    }
  }})
}

class PreferenceSignal[T] private (key: String, preferences: => SharedPreferences, default: T)(implicit val helper: Helper[T]) extends SourceSignal[T] with UiEventSource {
  import PreferenceSignal._
  private lazy val prefs = preferences

  disableAutowiring()

  onChanged { _ => SaveQueue ! this } (EventContext.Global)

  reload()

  private def reload() = Future { publish(helper.read(prefs, key, default), PreferenceExecutionContext) }

  private def writeCurrent(editor: SharedPreferences.Editor): Unit = helper.write(editor, key, currentValue.getOrElse(default))
}
