scalaVersion := "2.13.5"

lazy val commonSettings = Seq(
  organization := "io.github.pityka",
  version := "0.0.23",
  scalaVersion := "2.13.5",
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
  publishTo := sonatypePublishTo.value,
  fork := false
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

lazy val sharedJs = project
  .in(file("shared-js"))
  .settings(commonSettings)
  .settings(
    name := "nspl-shared-js",
    libraryDependencies += ("org.scala-js") %%% "scalajs-dom" % "1.0.0"
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
      ("org.scala-js") %%% "scalajs-dom" % "1.0.0",
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
      "org.scalatest" %% "scalatest" % "3.2.5" % "test"
    )
  )
  .dependsOn(core, sharedJvm)

lazy val scalatagsJvm = project
  .in(file("scalatags-jvm"))
  .settings(commonSettings)
  .settings(
    name := "nspl-scalatags-jvm",
    libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.7.0"
  )
  .dependsOn(core, sharedJvm)

lazy val saddle = (project in file("saddle"))
  .settings(commonSettings)
  .settings(
    name := "nspl-saddle",
    libraryDependencies ++= Seq(
      "io.github.pityka" %% "saddle-core" % "2.2.5",
      "org.scalatest" %% "scalatest" % "3.2.5" % "test"
    )
  )
  .dependsOn(core, awt, scalatagsJvm)

lazy val saddleJS = (project in file("saddle"))
  .settings(commonSettings)
  .settings(
    name := "nspl-saddle-js",
    target := file("saddle/targetJS"),
    libraryDependencies ++= Seq(
      "io.github.pityka" %%% "saddle-core" % "2.2.5"
    )
  )
  .dependsOn(coreJS)
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)

publishArtifact := false

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
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
