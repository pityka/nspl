---
title: 'Usage overview'
weight: 2
---

# Data sources
nspl plotting routines take data from `org.nspl.data.DataSource` subtypes. A DataSource is a lazy collection of vectors of doubles, with definition:

```scala mdoc
trait DataSource {
  def iterator: Iterator[Row]
}
trait Row {
  def apply(i: Int): Double
}
```



For most use cases the `import org.nspl._` imports implicit conversions from standard library types like
`Seq[(Double,Double)]` or tuples of other arity. 

There are a few concrete implementations of a DataSource in the `org.nspl.data` package like `DataTable` which is a row major matrix of doubles and `DataMatrix` which is a raster.

nspl is lazy in data. The iterator of the rows of a data source is run only when needed and the rows are only kept in memory where absolute necessary. For example, nspl can make raster plots without accumulating data in memory.

### Saddle data sources
Saddle is a data manipulation library for Scala, it provides features similar to R's data frames or python's pandas. 
The `"org.nspl" %% "nspl-saddle" % "@VERSION@"` module has adapters from Saddle data structures to nspl DataSource. 
Once on the class path you can import it with the `import org.nspl.saddle._` wildcard import, and then you can use an `org.saddle.Frame` everywhere where a DataSource is needed. 

# Plotting routines

nspl's API separates the definition of the plot from the actual rendering. 
First a data structures is created which describes the plot, then that structure is interpreted by a renderer.

## xyplot

The most important method in nspl's API is the `xyplot()()` method, a factory for descriptions of 2D plots in a Cartesian coordinate system. It takes two parameter lists: the first parameter list is for data sources and data renderers, the second parameter list is for global plot parameters.
Its signature is roughly:
```scala
def xyplot[F: FC](data: (DataSource, List[DataRenderer], LegendConfig)*)(
      xlog: Boolean = false,
      ylog: Boolean = false,
      main: String = "",
      xlab: String = "",
      ylab: String = "",
      .. many other global plot parameters ..)
```

In the first parameter list of `xyplot` we give one or more data sources each with zero or more `DataRenderer`. 

A `DataRenderer` is visual representation of a row or rows e.g. a point, a line segment, a horizontal or vertical bar, a boxplot, etc. A single data source may have multiple data renderes e.g. both a point and a line segment.
For the full list of data renderers see the methods in the [Renderers trait]({{< baseurl >}}/api/org/nspl/Renderers.html).

The third element of the triple controls whether the data source is included in the legend or not. 

In the below example we have a single data source, each row is plotted as a series of consecutive line segments and also as a point. Finally, the data source is included in the legend.
```scala mdoc:bytes:assets/usage1.png
import org.nspl._ 
import org.nspl.awtrenderer._ 

val someData = 
  List(
    0d -> 0d,
    1d -> 1d,
    2d -> 1d,
    3d -> 0d
  )

val plot = xyplot((someData , List(point(),line()), InLegend("series 1")))(
            xlab="x axis label",
            ylab="y axis label"
          )

renderToByteArray(plot.build, width=2000)
```




