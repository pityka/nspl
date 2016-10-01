[New Scala Plotting Library](https://pityka.github.io/nspl/)

[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-0.6.8.svg)](https://www.scala-js.org)

# 2d Plotting Library for Scala (including scala-js)

Creates scatter, line, contour, raster, barplots and boxplots, histograms, density plots. Composite figure of multiple plots.

Renders to
* java Graphics2D
* PDF/SVG/EPS (via [VectorGraphics2D](https://github.com/eseifert/vectorgraphics2d))
* SVG (via [ScalaTags](http://www.lihaoyi.com/scalatags/), both in browser or on jvm)
* Html5 Canvas ([scala-js](http://www.scala-js.org))

Minimal dependencies (VectorGraphics2D for PDF output).

Convenient interface for [SADDLE](https://saddle.github.io/) data structures.

Ability to use streams of data without keeping them in memory, thus can create plots from huge files to raster output.

Easily extendible, typesafe scene graph.

See `canvas/src/main/scala/org/nspl/test.scala` for Html5 Canvas examples.
See `saddle/src/test/scala/plots/plots.test.scala` for JVM examples.

## Usage

* JVM AWT backend: `libraryDependencies += "io.github.pityka" %% "nspl-awt" % "0.0.7"`
* JVM SVG via scalatags: `libraryDependencies += "io.github.pityka" %% "nspl-scalatags-jvm" % "0.0.7"`
* scala-js svg via scalatags: `libraryDependencies += "io.github.pityka" %% "nspl-scalatags-js" % "0.0.7"` 


## Limitations
* static plots (no user interaction)
* only fixed with font type for labels

## Alternatives

See the excellent java graphing library: [GRAL](https://github.com/eseifert/gral), which inspired the design of this library.
