package com.geteit.app

import android.app.Service
import android.content.{Context, Intent}
import android.os.{PowerManager, IBinder}
import android.support.v4.content.WakefulBroadcastReceiver
import com.geteit.concurrent.Threading
import com.geteit.util.Log._

import scala.concurrent.Future

abstract class FutureService extends Service {
  private implicit val logTag: LogTag = logTagFor[FutureService]

  override def onBind(intent: Intent): IBinder = null

  override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = {
    debug(s"onStartCommand: $startId, intent: $intent")

    val future = if (intent == null) Future.successful({}) else onIntent(intent, startId).recover { case ex => error("onIntent failed", ex) } (Threading.ui)
    future.onComplete { _ => onComplete(startId) }(Threading.ui)

    Service.START_REDELIVER_INTENT
  }

  protected def onIntent(intent: Intent, id: Int): Future[Any]

  protected def onComplete(startId: Int): Unit = {
    debug(s"onCompleted: $startId")
    stopSelf(startId)
  }
}

abstract class WakefulFutureService extends FutureService {

  val wakeLockLevel = PowerManager.PARTIAL_WAKE_LOCK

  lazy val powerManager = getApplicationContext.getSystemService(Context.POWER_SERVICE).asInstanceOf[PowerManager]
  private lazy val wakeLock = powerManager.newWakeLock(wakeLockLevel, getClass.getName)

  override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = {
    wakeLock.acquire()
    WakefulBroadcastReceiver.completeWakefulIntent(intent)

    super.onStartCommand(intent, flags, startId)
  }

  override protected def onComplete(startId: Int): Unit = {
    super.onComplete(startId)
    wakeLock.release()
  }
}
