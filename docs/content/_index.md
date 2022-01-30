---
title: 'nspl plotting library'
weight: 1
---

# Scala plotting library
nspl is a plotting library for the Scala programming language. 
nspl can create various plots (scatter, line, bar, box etc) and complex figures.

## Platforms and output formats
nspl supports Scala on the JVM and Scala.js.
It can render both vector graphics (SVG, PDF, postscript) and raster formats (JPG, PNG).
nspl also supports graphics contexts like `java.awt.Graphics2D` or the html5 canvas.


## Getting started
Add the following lines to your build.sbt depending on the runtime environment
```scala
// On JVM
libraryDependencies += "io.github.pityka" %% "nspl-awt" % "@VERSION@"
// In browser, using the canvas renderer
libraryDependencies += "io.github.pityka" %%% "nspl-canvas-js" % "@VERSION@"
// 
```

To use nspl on the JVM with the `java.awt` renderer you need the following imports:
```scala
import org.nspl._ 
import org.nspl.awtrenderer._ 
```

To use nspl in the browser with the html5 canvas renderer you need these imports:
```scala
import org.nspl._
import org.nspl.canvasrenderer._
```

For example, to draw a simple scatter plot:

```scala mdoc:bytes:assets/scatterplot.png
import org.nspl._ 
import org.nspl.awtrenderer._ 
import scala.util.Random.nextDouble

val someData = 
  0 until 100 map (_ => nextDouble() -> nextDouble())

val plot = xyplot(someData)(
            main="Main label", 
            xlab="x axis label",
            ylab="y axis label"
          )

renderToByteArray(plot.build, width=2000)
```

## Dependencies

nspl has minimal external dependencies.

- `nspl-awt` depends on [vectorgraphics2d](https://github.com/eseifert/vectorgraphics2d)
- `nspl-canvas-js` depends on the Scala.js runtime library and `scalajs-dom`

