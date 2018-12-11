scalaVersion := "2.12.6"

lazy val commonSettings = Seq(
  organization := "io.github.pityka",
  version := "0.0.20",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.11.12", "2.12.6"),
  javacOptions ++= Seq("-Xdoclint:none"),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  publishTo := sonatypePublishTo.value
)

lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "nspl-core"
  )
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

lazy val coreJS = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "nspl-core-js",
    target := file("core/targetJS"),
    sourceManaged in Compile := (sourceManaged in Compile).value.getAbsoluteFile
  )
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

lazy val coreNative = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.11.12",
    crossScalaVersions := Seq("2.11.12")
  )
  .settings(
    name := "nspl-core-native",
    target := file("core/targetNative"),
    sourceManaged in Compile := (sourceManaged in Compile).value.getAbsoluteFile
  )
  .enablePlugins(ScalaNativePlugin)
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

lazy val nanovg = project
  .in(file("nanovg"))
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.11.12",
    crossScalaVersions := Seq("2.11.12"),
    nativeLinkingOptions ++= Seq("-framework", "OpenGL")
  )
  .settings(
    name := "nspl-nanovg-native"
  )
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(coreNative)

lazy val cli = project
  .in(file("cli"))
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.11.12",
    crossScalaVersions := Seq("2.11.12"),
    nativeLinkingOptions ++= Seq("-framework", "OpenGL"),
    nativeLinkStubs := true,
    libraryDependencies ++= Seq(
      ("com.lihaoyi") %%% "fastparse" % "1.0.0",
      ("io.github.pityka") %%% "stringsplit" % "1.0.1",
      ("com.lihaoyi") %%% "utest" % "0.6.2" % "test"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .settings(
    name := "nsplcli"
  )
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(nanovg)

lazy val sharedJs = project
  .in(file("shared-js"))
  .settings(commonSettings)
  .settings(
    name := "nspl-shared-js",
    libraryDependencies += toScalaJSGroupID("org.scala-js") %%% "scalajs-dom" % "0.9.2"
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS)

lazy val canvas = project
  .in(file("canvas"))
  .settings(commonSettings)
  .settings(
    name := "nspl-canvas-js",
    libraryDependencies += toScalaJSGroupID("org.scala-js") %%% "scalajs-dom" % "0.9.2"
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS, sharedJs)

lazy val scalatagsJs = project
  .in(file("scalatags-js"))
  .settings(commonSettings)
  .settings(
    name := "nspl-scalatags-js",
    libraryDependencies ++= Seq(
      toScalaJSGroupID("org.scala-js") %%% "scalajs-dom" % "0.9.2",
      toScalaJSGroupID("com.lihaoyi") %%% "scalatags" % "0.6.7")
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS, sharedJs)

lazy val sharedJvm = project
  .in(file("shared-jvm"))
  .settings(commonSettings)
  .settings(
    name := "nspl-shared-jvm"
  )
  .dependsOn(core)

lazy val awt = project
  .in(file("awt"))
  .settings(commonSettings)
  .settings(
    name := "nspl-awt",
    libraryDependencies ++= Seq(
      "de.erichseifert.vectorgraphics2d" % "VectorGraphics2D" % "0.13",
      "org.scalatest" %% "scalatest" % "3.0.0" % "test")
  )
  .dependsOn(core, sharedJvm)

lazy val scalatagsJvm = project
  .in(file("scalatags-jvm"))
  .settings(commonSettings)
  .settings(
    name := "nspl-scalatags-jvm",
    libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.7"
  )
  .dependsOn(core, sharedJvm)

lazy val saddle = (project in file("saddle"))
  .settings(commonSettings)
  .settings(
    crossScalaVersions := Seq("2.11.12"),
    name := "nspl-saddle",
    libraryDependencies ++= Seq(
      "io.github.pityka" %% "saddle-core-fork" % "1.3.4-fork1" exclude ("com.googlecode.efficient-java-matrix-library", "ejml"),
      "com.googlecode.efficient-java-matrix-library" % "ejml" % "0.19" % "test",
      "org.scalatest" %% "scalatest" % "3.0.0" % "test"
    )
  )
  .dependsOn(core, awt, scalatagsJvm)

publishArtifact := false

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(saddle,
             scalatagsJvm,
             awt,
             scalatagsJs,
             canvas,
             sharedJs,
             sharedJvm,
             core,
             coreJS)

pomExtra in Global := {
  <url>https://pityka.github.io/nspl/</url>
  <scm>
    <connection>scm:git:github.com/pityka/nspl</connection>
    <developerConnection>scm:git:git@github.com:pityka/nspl</developerConnection>
    <url>github.com/pityka/nspl</url>
  </scm>
  <developers>
    <developer>
      <id>pityka</id>
      <name>Istvan Bartha</name>
      <url>https://pityka.github.io/nspl/</url>
    </developer>
  </developers>
}

scalafmtOnCompile in ThisBuild := true
