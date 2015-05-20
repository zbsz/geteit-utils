import android.Keys._

android.Plugin.androidBuildAar

platformTarget in Android := "android-22"

name := "geteit-utils"

organization := "com.geteit"

version := "0.1"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-feature")

crossScalaVersions := Seq("2.10.0", "2.11.6")

libraryDependencies ++= Seq(
  "com.android.support" % "support-v4" % "21.0.0",
  "org.robolectric" % "android-all" % "5.0.0_r2-robolectric-0" % Test,
  "org.scalatest" %% "scalatest" % "2.2.1" % Test,
  "org.scalacheck" %% "scalacheck" % "1.11.6" % Test
)

publishTo := {
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some(Resolver.file("snapshots", new File("../mvn-repo/snapshots" )) )
  else
    Some(Resolver.file("releases", new File("../mvn-repo/releases" )) )
}
