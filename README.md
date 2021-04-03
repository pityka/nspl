[New Scala Plotting Library](https://pityka.github.io/nspl/)

[![](https://img.shields.io/maven-central/v/io.github.pityka/nspl-core_2.12.svg)](https://search.maven.org/search?q=g:io.github.pityka%20nspl)



# 2d Plotting Library for Scala (including scala-js and scala-native)

Creates scatter, line, contour, raster, barplots and boxplots, histograms, density plots. Composite figure of multiple plots.

Renders to
* java Graphics2D
* PDF/SVG/EPS (via [VectorGraphics2D](https://github.com/eseifert/vectorgraphics2d))
* SVG (via [ScalaTags](http://www.lihaoyi.com/scalatags/), both in browser or on jvm)
* Html5 Canvas ([scala-js](http://www.scala-js.org))
* OpenGL (via [NanoVG](https://github.com/memononen/nanovg))

Minimal dependencies (VectorGraphics2D for PDF output).

Convenient interface for [SADDLE](https://saddle.github.io/) data structures.

Ability to use streams of data without keeping them in memory, thus can create plots from huge files to raster output.

Easily extendible, typesafe scene graph.

Text labels may use any font accessible to the JVM or the browser. Font widths are properly measured.

Simple user interaction: panning and zooming.

See `canvas/src/main/scala/org/nspl/test.scala` for Html5 Canvas examples.
See `saddle/src/test/scala/plots/plots.test.scala` for JVM examples.

## Usage

* JVM AWT backend: `libraryDependencies += "io.github.pityka" %% "nspl-awt" % "0.0.???"`
* JVM SVG via scalatags: `libraryDependencies += "io.github.pityka" %% "nspl-scalatags-jvm" % "0.0.???"`
* scala-js svg via scalatags: `libraryDependencies += "io.github.pityka" %% "nspl-scalatags-js" % "0.0.???"` 


## Alternatives

See the excellent java graphing library: [GRAL](https://github.com/eseifert/gral), which inspired the design of this library.

## Scala Native
You will need lodepng, glfw3 and nanovg. For nanovg compile and install a file like this:
```
#define GLFW_INCLUDE_GLCOREARB
#define GLFW_INCLUDE_GLEXT
#include <GLFW/glfw3.h>
#include "nanovg.h"
#define NANOVG_GL3_IMPLEMENTATION
#include "nanovg_gl.h"
```

In `cli/` there is an experimental command line interface.
