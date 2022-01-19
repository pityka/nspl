---
title: 'Getting started'
weight: 1
---

Add any of these lines to your build.sbt:
```scala
// On JVM
libraryDependencies += "io.github.pityka" %% "nspl-awt" % "@VERSION@"
// In browser, using the canvas renderer
libraryDependencies += "io.github.pityka" %%% "nspl-canvas-js" % "@VERSION@"
// 
```

### Dependencies

- `nspl-awt` depends on [vectorgraphics2d](https://github.com/eseifert/vectorgraphics2d)
- `nspl-canvas-js` has no external dependencies besides the `scalajs-dom` from the Scala Js project.

### Example: 
```scala mdoc
import scala.io.Source

```