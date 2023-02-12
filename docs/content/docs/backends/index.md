---
title: 'Output formats'
weight: 5
---

Nspl has support for the following output formats and platforms:

1. The `"io.github.pityka" %% "nspl-awt"`  module works on the JVM and may produce PDF, SVG, EPS, PNG, JPG encoded images or documents, or draws into the interactive Java Graphics2D context).
2. The `"io.github.pityka" %%% "nspl-canvas-js"` module runs in the browser and can draw onto an interactive html5 canvas.
3. The `"io.github.pityka" %%% "scalatags-js"` module can produce an SVG image as a DOM subtree (using ScalaTags).
4. The `"io.github.pityka" %% "scalatags-jvm"` module can produce SVG documents using ScalaTags.

# AWT backend (JVM)

Add this to your build.sbt:
```scala
libraryDependencies += "io.github.pityka" %% "nspl-awt" % "@VERSION@"
```

You can use the methods in `org.nspl.awtrenderer` to produce plots, e.g.
`pdfToFile`, `pngToFile`, `pngToByteArray` etc. See this section of the API documentation: [awtrenderer object]({{< baseurl >}}/api/org/nspl/awtrenderer$.html) for all the variations of these.

# canvas backend (Scala.js)

nspl has a high performance, high quality renderer for the html5 canvas. 
It can deliver smooth framerates up to several thousand data points.
It also has good visual quality, e.g. on high DPI screens we render to an appropriately oversized raster.

The rendered plot receives and reacts to mouse events which makes it suitable for interactive use.

Add this to your build.sbt:
```scala
libraryDependencies += "io.github.pityka" %%% "nspl-canvas-js" % "@VERSION@"
```

You can use the `org.nspl.canvasrenderer.render` method to create a new canvas which you can then add to your DOM tree. 

## Interactive use (callbacks)
The backend supports basic user interaction like zooming and panning. 
The canvas renderer backend draws a few thousand data points with 60fps.

The `org.nspl.canvasrenderer.render` method takes callback which is called whenever the user interacts with the plot on the canvas. 
Furthermore the `render` method returns a 2-tuple: the first element is the canvas, the second is a lambda which you can use to update the plot. This lambda takes a renderable element from nspl.

The below example renders a plot which forwards events to DOM and receives events from the DOM.

```scala mdoc:js
<div id="log"></div>
<button type="button" id="button">Click me</button>
---
import org.nspl._ 
import org.nspl.canvasrenderer._
import scala.util.Random.nextDouble
import org.scalajs.dom.document
import org.scalajs.dom.MouseEvent

def someData() = 
  0 until 100 map (_ => nextDouble() -> nextDouble())

def makePlot() = xyplot(someData())(par)

val logDiv = document.getElementById("log")
val button = document.getElementById("button")
                .asInstanceOf[org.scalajs.dom.HTMLElement]

val (canvas, updatePlotCallback) = render(makePlot(),
                             width = 600, height = 600,
                             click = { _ => 
                                logDiv.textContent = "Plot clicked"
                              }
                            )

val onClick : MouseEvent => Unit = { (_:MouseEvent) =>
  updatePlotCallback(makePlot())
}                            

button.onclick = onClick
node.appendChild(canvas)

```

# scalatags backends

The JVM and Scala.js scalatags backends work similarly: you can use the `org.nspl.scalatagrenderer.renderToScalaTag` method to turn the nspl renderable element into a ScalaTag tree. 

Add one of these to your build.sbt:
```scala
// for scala.js
libraryDependencies += "io.github.pityka" %%% "nspl-scalatags-js" % "@VERSION@"
// or for the jvm
libraryDependencies += "io.github.pityka" %% "nspl-scalatags-jvm" % "@VERSION@"
```

