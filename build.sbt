scalaVersion := "2.13.6"

inThisBuild(
  List(
    organization := "io.github.pityka",
    homepage := Some(url("https://pityka.github.io/nspl/")),
    licenses := List(("MIT", url("https://opensource.org/licenses/MIT"))),
    developers := List(
      Developer(
        "pityka",
        "Istvan Bartha",
        "bartha.pityu@gmail.com",
        url("https://github.com/pityka/nspl")
      )
    )
  )
)

lazy val commonSettings = Seq(
  scalaVersion := "2.13.6",
  crossScalaVersions := Seq("2.12.15", "2.13.6"),
  javacOptions ++= Seq("-Xdoclint:none"),
  scalacOptions ++= Seq(
    "-language:postfixOps",
    "-encoding",
    "utf8", // Option and arguments on same line
    "-Xfatal-warnings", // New lines for each options
    "-language:implicitConversions",
    "-deprecation",
    "-unchecked",
    "-feature"
  ),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  fork := false
)

lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "nspl-core",
    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-compat" % "2.5.0"
  )
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

lazy val coreJS = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "nspl-core-js",
    target := file("core/targetJS"),
    sourceManaged in Compile := (sourceManaged in Compile).value.getAbsoluteFile,
    libraryDependencies += "org.scala-lang.modules" %%% "scala-collection-compat" % "2.5.0"
  )
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

lazy val sharedJs = project
  .in(file("shared-js"))
  .settings(commonSettings)
  .settings(
    name := "nspl-shared-js",
    libraryDependencies += ("org.scala-js") %%% "scalajs-dom" % "1.2.0"
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS)

lazy val canvas = project
  .in(file("canvas"))
  .settings(commonSettings)
  .settings(
    name := "nspl-canvas-js"
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS, sharedJs)

lazy val scalatagsJs = project
  .in(file("scalatags-js"))
  .settings(commonSettings)
  .settings(
    name := "nspl-scalatags-js",
    libraryDependencies ++= Seq(
      ("org.scala-js") %%% "scalajs-dom" % "1.2.0",
      ("com.lihaoyi") %%% "scalatags" % "0.9.1"
    )
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
      "org.scalatest" %% "scalatest" % "3.2.10" % "test"
    )
  )
  .dependsOn(core, sharedJvm)

lazy val scalatagsJvm = project
  .in(file("scalatags-jvm"))
  .settings(commonSettings)
  .settings(
    name := "nspl-scalatags-jvm",
    libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.9.4"
  )
  .dependsOn(core, sharedJvm)

lazy val saddle = (project in file("saddle"))
  .settings(commonSettings)
  .settings(
    name := "nspl-saddle",
    libraryDependencies ++= Seq(
      "io.github.pityka" %% "saddle-core" % "2.2.8",
      "org.scalatest" %% "scalatest" % "3.2.10" % "test"
    )
  )
  .dependsOn(core, awt, scalatagsJvm)

lazy val saddleJS = (project in file("saddle"))
  .settings(commonSettings)
  .settings(
    name := "nspl-saddle-js",
    target := file("saddle/targetJS"),
    libraryDependencies ++= Seq(
      "io.github.pityka" %%% "saddle-core" % "2.2.8"
    )
  )
  .dependsOn(coreJS)
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

publishArtifact := false

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(crossScalaVersions := Nil, skip in publish := true)
  .aggregate(
    saddle,
    saddleJS,
    scalatagsJvm,
    awt,
    canvas,
    sharedJs,
    sharedJvm,
    core,
    coreJS
  )
