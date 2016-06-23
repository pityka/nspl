[New Scala Plotting Library](https://pityka.github.io/nspl/)

[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-0.6.8.svg)](https://www.scala-js.org)

# 2d Plotting Library for Scala (including scala-js)

Renders to
* java Graphics2D
* PDF/SVG/EPS (via [VectorGraphics2D](https://github.com/eseifert/vectorgraphics2d))
* Html5 Canvas ([scala-js](http://www.scala-js.org))

Creates scatter, line, contour, raster, barplots and boxplots, histograms, density plots. Creates composite figure of multiple plots

Minimal dependencies (VectorGraphics2D for PDF output).

Convenient interface for [SADDLE](https://saddle.github.io/) data structures.

Can work from streams of data without keeping them in memory, thus can create plots from huge files to raster output.

Easily extendible, typesafe scene graph.

See `canvas/src/main/scala/org/nspl/test.scala` for Html5 Canvas examples.
See `saddle/src/test/scala/plots/plots.test.scala` for JVM examples.
