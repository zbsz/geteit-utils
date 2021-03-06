import android.Keys._
import sbt.Keys._

lazy val root = Project("geteit-utils", file("."))
  .settings(android.Plugin.androidBuildAar: _*)
  .settings(buildSettings: _*)
  .settings(
    name := "geteit-utils",
    platformTarget in Android := "android-22",
    transitiveAndroidLibs in Android := true,
    libraryDependencies ++= Seq(
      "com.android.support" % "support-v4" % "22.0.0",
      "org.scalatest" %% "scalatest" % "2.2.1" % Test,
      "org.scalacheck" %% "scalacheck" % "1.11.6" % Test,
      "com.geteit" %% "robotest" % "0.+" % Test
    )
  )
  .aggregate(macros)
  .dependsOn(macros)

lazy val macros = project
  .settings(buildSettings: _*)
  .settings(macroProjectSettings: _*)
  .settings(
    name := "geteit-util-macros",
    libraryDependencies ++= Seq(
      "com.google.code.gson" % "gson" % "2.3.1",
      "org.robolectric" % "android-all" % "5.0.0_r2-robolectric-0" % Provided
    )
  )

lazy val buildSettings = Seq(
  organization := "com.geteit",
  version := "0.4",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.5", "2.11.7"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked"
  ),
  resolvers ++= Seq(
    Resolver.mavenLocal,
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  ),
  fork in Test := true,
  publishArtifact in Test := false,
  javaOptions in Test ++= Seq("-XX:MaxPermSize=2048M", "-XX:+CMSClassUnloadingEnabled"),
  javacOptions ++= Seq("-source", "1.7", "-target", "1.7"),
  scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-language:postfixOps", "-target:jvm-1.7"),

  /** We need the Macro Paradise plugin both to support the macro
    * annotations used in the public type provider implementation and to
    * allow us to use quasiquotes in both implementations. The anonymous
    * type providers could easily (although much less concisely) be
    * implemented without the plugin.
    */
  addCompilerPlugin(paradiseDependency)
)

lazy val paradiseDependency = "org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full

lazy val macroProjectSettings = Seq(
  libraryDependencies <+= (scalaVersion)(
    "org.scala-lang" % "scala-reflect" % _
  ),
  libraryDependencies ++= (
    if (scalaVersion.value.startsWith("2.10")) List(paradiseDependency) else Nil
  )
)
