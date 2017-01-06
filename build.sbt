scalaVersion := "2.11.8"

lazy val commonSettings = Seq(
  organization := "io.github.pityka",
  version := "0.0.14-SNAPSHOT",
  scalaVersion := "2.11.8",
  javacOptions ++= Seq("-Xdoclint:none"),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
)

lazy val core = project.in(file("core")).
		settings(commonSettings).
		settings(
			name:="nspl-core"
		)
    .enablePlugins(spray.boilerplate.BoilerplatePlugin)

lazy val coreJS = project.in(file("core")).
		settings(commonSettings).
		settings(
			name:="nspl-core-js",
      target:= file("core/targetJS"),
      sourceManaged in Compile := (sourceManaged in Compile).value.getAbsoluteFile
		)
    .enablePlugins(ScalaJSPlugin)
    .enablePlugins(spray.boilerplate.BoilerplatePlugin)


lazy val canvas = project.in(file("canvas")).
		settings(commonSettings).
		settings(
			name:="nspl-core-canvas-js",
      libraryDependencies +=  "org.scala-js" %%% "scalajs-dom" % "0.9.0"
		)
    .enablePlugins(ScalaJSPlugin)
    .dependsOn(coreJS)

lazy val scalatagsJs = project.in(file("scalatags-js")).
		settings(commonSettings).
		settings(
			name:="nspl-scalatags-js",
      libraryDependencies ++=  Seq(
        "org.scala-js" %%% "scalajs-dom" % "0.9.1",
        "com.lihaoyi" %%% "scalatags" % "0.6.0")
		)
    .enablePlugins(ScalaJSPlugin)
    .dependsOn(coreJS)

lazy val awt = project.in(file("awt")).
		settings(commonSettings).
		settings(
			name:="nspl-awt",
      libraryDependencies += "de.erichseifert.vectorgraphics2d" % "VectorGraphics2D" % "0.11"
		).dependsOn(core)

lazy val scalatagsJvm = project.in(file("scalatags-jvm")).
    		settings(commonSettings).
    		settings(
    			name:="nspl-scalatags-jvm",
          libraryDependencies +="com.lihaoyi" %% "scalatags" % "0.6.0"
    		).dependsOn(core)

lazy val saddle = (project in file("saddle")).settings(commonSettings).
	settings(
		name:="nspl-saddle",
		libraryDependencies++=Seq(
			"org.scala-saddle" %% "saddle-core" % "1.3.4"  exclude("com.googlecode.efficient-java-matrix-library", "ejml"),
			"com.googlecode.efficient-java-matrix-library" % "ejml" % "0.19" % "test",
      "org.scalatest" %% "scalatest" % "2.1.5" % "test"
			)
	).dependsOn(core,awt,scalatagsJvm)
	.aggregate(core,awt,coreJS,canvas,scalatagsJs,scalatagsJvm)

onLoad in Global := (Command.process("project saddle", _: State)) compose (onLoad in Global).value

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
