---
title: 'Saddle Documentation'
weight: 1
---

```scala
// Java Graphics2D:
"io.github.pityka" %% "nspl-awt" % "@VERSION@"

// Html5 Canvas
"io.github.pityka" %%% "nspl-canvas-js" % "@VERSION@"

```

```scala mdoc:bytes:assets/scatterplot.png
import org.nspl._ 
import org.nspl.awtrenderer._ 

val plot = xyplot(List(1d -> 1d))()

renderToByteArray(plot.build)

```