[New Scala Plotting Library](https://pityka.github.io/nspl/)

[![](https://img.shields.io/maven-central/v/io.github.pityka/nspl-core_2.12.svg)](https://search.maven.org/search?q=g:io.github.pityka%20nspl)



# 2d Plotting Library for Scala (including scala-js)

Creates scatter, line, contour, raster, barplots and boxplots, histograms, density plots. Composite figure of multiple plots. 

Renders to
* java Graphics2D
* PDF/SVG/EPS (via [VectorGraphics2D](https://github.com/eseifert/vectorgraphics2d))
* SVG (via [ScalaTags](http://www.lihaoyi.com/scalatags/), both in browser or on jvm)
* Html5 Canvas ([scala-js](http://www.scala-js.org))

Minimal dependencies (VectorGraphics2D for PDF output).

May use streams of data, and create plots from huge files to raster output.

Text labels may use any font accessible to the JVM or the browser. 

Simple user interaction: panning and zooming.

See `canvas/src/main/scala/org/nspl/test.scala` for Html5 Canvas examples.
See `saddle/src/test/scala/plots/plots.test.scala` for JVM examples.

## Usage

* JVM AWT backend: `libraryDependencies += "io.github.pityka" %% "nspl-awt" % "0.0.???"`
* JVM SVG via scalatags: `libraryDependencies += "io.github.pityka" %% "nspl-scalatags-jvm" % "0.0.???"`
* scala-js svg via scalatags: `libraryDependencies += "io.github.pityka" %% "nspl-scalatags-js" % "0.0.???"` 
* scala-js canvas via scalatags: `libraryDependencies += "io.github.pityka" %% "nspl-canvas-js" % "0.0.???"` 

## Version policy

nspl uses 'early-semver' versioning enforced with https://github.com/scalacenter/sbt-version-policy. 

## Alternatives

See the excellent java graphing library: [GRAL](https://github.com/eseifert/gral), which inspired the design of this library.
