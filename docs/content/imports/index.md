---
title: 'Imports'
weight: 3
---

# Imports

## Wildcard imports

If you decide to use wildcard imports then import everthing from the `org.nspl` package. 
This will import all the plotting routines, but will not import any platform specific code.

Most plotting routines need a `org.nspl.FontConfiguration` instance in the implicit scope. 
This instance is platform specific, and if you import everything from one of the platform specific packages then this will be imported as well in the scope.

In summary, with wildcard imports you need these two imports if using the canvas backend:
```scala
import org.nspl._
import org.nspl.canvasrenderer._
```

.. and these two if you use the JVM AWT backend:
```scala
import org.nspl._ 
import org.nspl.awtrenderer._ 
```

## Non-wildcard imports

If you don't want to pollute the namespace then you can import only the `FontConfiguration` implicit instance, and prefix every call with its package e.g.

```scala
import org.nspl
import org.nspl.awtrenderer.{defaultGlyphMeasurer, defaultAWTFont}
```