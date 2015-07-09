package com.geteit

import android.app.{ActivityManager, Activity, Application}
import android.content.{ContentResolver, ContextWrapper, Context}
import android.support.v4.app.{FragmentActivity, FragmentManager}
import com.geteit.app.{GtContext, GtApplication}
import com.geteit.events.EventContext

package object inject {

  def GtModule = Module { bind =>
    val ctx = GtApplication.APP_INSTANCE
    bind [Context] to ctx
    bind [Application] to ctx
    bind [EventContext] to EventContext.Global
    bind [ContentResolver] to ctx.getContentResolver
    bind [ActivityManager] to ctx.getSystemService(Context.ACTIVITY_SERVICE).asInstanceOf[ActivityManager]
  }

  def GtContextModule(ctx: GtContext) = Module { implicit bind =>

    bind [Context] to ctx
    bind [GtContext] to ctx
    bind [EventContext] to ctx.eventContext
    bind [Activity] to {
      def getActivity(ctx: Context): Activity = ctx match {
        case a: Activity => a
        case w: ContextWrapper => getActivity(w.getBaseContext)
      }
      getActivity(ctx)
    }
    bind [FragmentManager] to bind.inject[Activity].asInstanceOf[FragmentActivity].getSupportFragmentManager
  }
}
