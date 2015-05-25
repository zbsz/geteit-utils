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
      "org.robolectric" % "android-all" % "5.0.0_r2-robolectric-0" % Provided,
      "com.geteit" %% "robotest" % "0.7" % Test,
      "junit" % "junit" % "4.8.2" % Test
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
  version := "0.3-SNAPSHOT",
  scalaVersion := "2.11.6",
  crossScalaVersions := Seq("2.10.5", "2.11.6"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked"
  ),
  resolvers ++= Seq(
    "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository",
    "RoboTest releases" at "https://raw.github.com/zbsz/mvn-repo/master/releases/",
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  ),
  publishTo := {
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some(Resolver.file("snapshots", new File("../mvn-repo/snapshots" )) )
    else
      Some(Resolver.file("releases", new File("../mvn-repo/releases" )) )
  },
  fork in Test := true,
  publishArtifact in Test := false,
  javaOptions in Test ++= Seq("-XX:MaxPermSize=2048M", "-XX:+CMSClassUnloadingEnabled"),

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
