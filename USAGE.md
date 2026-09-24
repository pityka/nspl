---
name: nspl-usage
description: >
  Use the nspl plotting library as a dependency to create charts from Scala or
  Scala.js: scatter, line, bar, histogram, box, heatmap/raster, contour, and
  density plots, plus multi-plot figures. Covers dependency setup, the minimal
  API model, a plot-type cookbook, configuring plots with `par`, colors and
  colormaps, composing figures, rendering to PNG/PDF/SVG on the JVM, and the
  interactive Canvas/SVG backends in the browser. Load this when someone wants
  to *make a plot with* nspl. To modify nspl itself, use SKILL.md instead.
---

# Using nspl

nspl turns data into a 2D plot. You describe the plot with a small DSL, then
hand it to a backend that renders it — to an image or vector file on the JVM, or
to an interactive Canvas / SVG element in the browser. This guide is for
*consuming* nspl. If you are changing nspl's own source, read `SKILL.md`.

Full API docs: https://pityka.github.io/nspl/ — the option list lives in
`core/src/main/scala/org/nspl/Parameters.scala`, and
`saddle/src/test/scala/plots/plots.test.scala` is a large worked gallery.

## 1. Add the dependency

Organization is `io.github.pityka`; artifacts are cross-published for Scala 2.13
and Scala 3. Use the latest version from the Maven Central badge on the project
README (shown as `<version>` below). Pick the artifact for your target:

| You want to… | Dependency |
| --- | --- |
| Create plots on the JVM, save PNG/JPG/PDF/SVG/EPS | `"io.github.pityka" %% "nspl-awt" % "<version>"` |
| Plot Saddle `Vec`/`Series`/`Mat`/`Frame` on the JVM | `"io.github.pityka" %% "nspl-saddle" % "<version>"` (pulls in `nspl-awt`) |
| Interactive HTML5 Canvas (Scala.js) | `"io.github.pityka" %%% "nspl-canvas-js" % "<version>"` |
| Interactive inline SVG (Scala.js) | `"io.github.pityka" %%% "nspl-svg-js" % "<version>"` |
| The plot model only, no renderer (rare) | `"io.github.pityka" %% "nspl-core" % "<version>"` |

Scala.js modules use `%%%`. `nspl-awt` needs no extra setup — it bundles PDF/SVG/EPS
export via VectorGraphics2D.

## 2. The whole model, in five facts

1. `import org.nspl._` brings the DSL. Import **one backend** too — that import
   supplies the implicit renderers and default font, so without it your plot
   will not compile or render.
2. A plot factory such as `xyplot(...)` returns a `Build` (an interaction-aware
   description). Call `.build` to get the finished, immutable plot; hand *that*
   to a JVM output helper. In the browser, pass the `Build` straight to `render`.
3. Data goes in as a `DataSource`, but you almost never construct one — implicit
   conversions turn `Seq[(Double, Double)]`, tuples, and Saddle types into one.
4. `data -> renderer` pairs a data source with how to draw it (`point()`,
   `line()`, `bar()`, …). `xyplot` takes one or more such pairs.
5. `par` is the configuration object. Every setting is a builder copy:
   `par.xlab("x").ylab("y").main("Title").ylog(true)`. Sizes are relative to
   font size (write `1 fts`, `0.5 fts`), not pixels.

## 3. Quick start

JVM — render a scatter plot to a PNG and a PDF:

```scala
import org.nspl._
import org.nspl.awtrenderer._

val data = Seq(0d -> 0d, 1d -> 1d, 2d -> 4d, 3d -> 9d)

val plot = xyplot(data -> point())(
  par.xlab("x").ylab("y").main("Squares")
)

val png: java.io.File   = pngToFile(plot.build)
val pdf: java.io.File   = pdfToFile(plot.build)
val bytes: Array[Byte]  = pngToByteArray(plot.build, width = 1200)
```

Browser (Scala.js) — an interactive, pan/zoom/hover-able chart:

```scala
import org.nspl._
import canvasrenderer._
import org.scalajs.dom.document

val plot = xyplot(
  (1 to 50).map(i => (i.toDouble, math.sin(i * 0.2))) -> line()
)(par.xlab("i").ylab("sin"))

val (node, update) = render(plot, width = 600, height = 400, enableCrosshair = true)
document.body.appendChild(node)
```

Swap `import canvasrenderer._` for `import svgrenderer._` to render inline SVG
instead — the `render` signature is identical.

## 4. Getting data in

Any of these become a `DataSource` implicitly (just use them where a data source
is expected):

```scala
Seq(0d -> 1d, 1d -> 3d, 2d -> 2d)                 // (x, y) rows
Seq((0d, 1d, 0d), (1d, 3d, 1d))                   // (x, y, z) — 3rd column drives color/size/etc.
indexed(Seq(3.1, 2.7, 4.0))                       // (index, value) rows
```

With Saddle (needs `nspl-saddle` and `import org.nspl.saddle._`):

```scala
import org.nspl._
import org.nspl.saddle._
import org.nspl.awtrenderer._
import org.saddle._

val frame: Frame[Int, String, Double] = ???
xyplot(frame.col("height", "weight"))(par).build
```

`Vec`, `Series`, `Mat`, and `Frame` all convert; a `Frame`/`Mat` exposes each
column, so `xyplot(frame.col("x","y"))` plots column *x* against column *y*.

## 5. Plot-type cookbook

All of these return a `Build`; call `.build` and render as in the quick start.

```scala
xyplot(data -> point())(par)                                  // scatter
xyplot(data -> line())(par)                                   // line
xyplot(data -> point(), data -> line())(par)                  // overlay both

xyplot(                                                        // color by 3rd column
  Seq((0d,0d,0d), (1d,1d,1d), (2d,2d,2d)) -> point(color = DiscreteColors(3))
)(par)

xyplot(                                                        // multi-series + legend
  (seriesA, line(color = Color.red),   InLegend("model")),
  (seriesB, point(color = Color.blue), InLegend("data"))
)(par.main("Comparison"))

xyplot(HistogramData(values, 30) -> bar())(par.xlab("value").ylab("freq"))   // histogram

boxplot(frame)(par)                                           // one box per Frame column
boxplotFromLabels(Seq("A" -> 1d, "A" -> 2d, "B" -> 5d))(par)  // box per label group

xyplot(density(values) -> line())(par.ylab("density"))        // 1D density (KDE)

rasterplotFromFrame(frame)(par)                               // heatmap of a Frame

contourplot(                                                  // contour of a function
  xlim = (-2d, 2d), ylim = (-2d, 2d),
  f = (x, y) => x * x + y * y, n = 50, levels = 10
)(par)
```

For a heatmap from a plain array rather than a Saddle `Frame`, build a
`DataSource` with `rasterFromSeq(values, numCols, numRows)` and pass it to
`rasterplot(source, HeatMapColors(min, max))(par)`.

## 6. Configuring the plot with `par`

`par` starts from defaults; chain copies to change settings. Common ones (see
`Parameters.scala` for the full set — each has both an `x(v)` and a `withX(v)`
spelling):

```scala
par
  .main("Title").xlab("x axis").ylab("y axis")
  .xlog(true).ylog(true)                       // log10 axes (data must be > 0)
  .xlim(Some(0d -> 10d)).ylim(Some(0d -> 100d)) // fixed ranges (None = data-driven)
  .xnames(Seq(0d -> "low", 1d -> "high"))       // custom tick labels at positions
  .xgrid(true).ygrid(true)
  .xLabelRotation(-0.5)                          // radians
  .noLegend(true)
  .xWidth(30 fts).yHeight(20 fts)               // plot-area size, font-relative
```

## 7. Renderer options (how marks look)

The `data -> renderer` pairing controls appearance. Renderers read extra data
columns by index, so aesthetics can be data-driven:

```scala
point(size = 6d, color = Color.red)
point(color = DiscreteColors(5), colorCol = 2)        // color from column index 2
point(shapes = Vector(Shape.circle(1), Shape.square(1)), shapeCol = 3)
point(valueText = true)                               // print each point's value
point(errorBarColor = Color.gray2, errorBarStroke = StrokeConf(0.3 fts))

line(color = Color.blue, stroke = StrokeConf(2 fts))
bar(width = 0.8, fill = Color.gray4)
bar(horizontal = true, fill = RedBlue(-2, 2), fillCol = 1)
boxwhisker(fill = Color.gray4, width = 1d)
```

## 8. Colors and colormaps

`Color(r, g, b)` or `Color(r, g, b, a)` (0–255), plus named constants
(`Color.red`, `Color.gray4`, `Color.transparent`, …) and `colorFromHexString("ff8800")`.

A `Colormap` maps a `Double` to a color — pass one anywhere a `color`/`fill`
takes a colormap to color by value:

- `DiscreteColors(n)` — categorical palette for `n` classes.
- `HeatMapColors(min, max)`, `LogHeatMapColors(min, max)` — sequential.
- `RedBlue(min, max)`, `GrayScale(min, max)` — diverging / grayscale.
- `TableColormap(...)`, `ManualColor(map, default)` — explicit value→color.

`NaN` maps to transparent in the built-in colormaps, which is how you leave a
raster cell or point blank.

## 9. Composing several plots into one figure

Any finished plot is itself a drawable element, so combine them with `group`
(fixed small count) or `sequence` (a `Seq`) under a layout, then render the
group:

```scala
val figure = group(scatterPlot, linePlot, histogramPlot, TableLayout(2))
pngToFile(figure.build, width = 2000)
```

Layouts: `TableLayout(columns)`, `ColumnLayout(rows)`, `VerticalStack`,
`HorizontalStack`, `ZStack`, `FreeLayout`. Use `fitToWidth`/`fitToHeight`/
`fitToBounds` to size an element. You can drop in `TextBox("caption")` and
`ShapeElem(Shape.circle(3))` as elements too.

## 10. Rendering and output

**JVM** (`import org.nspl.awtrenderer._`). Each takes the built plot (or a
`Build`) and a `width`; height follows from the plot's aspect ratio.

```scala
pngToFile(plot.build)                       // File; also pngToFile(plot.build, width = 2000)
pdfToFile(plot.build, textAsShapes = false) // keep selectable text in the PDF
svgToFile(f, plot.build, width = 800, textAsShapes = true)
renderToByteArray(plot.build, width = 1000, mimeType = "image/png")
show(plot.build)                            // live Swing preview window
```

`textAsShapes = true` outlines glyphs (portable, no font dependency);
`false` keeps real, selectable text.

**Browser** (`import canvasrenderer._` or `svgrenderer._`):

```scala
val (node, update) = render(
  plot, width = 600, height = 400,
  onShapeClick = Some((id, pt, ev) => println(s"clicked $id")),
  onHover      = Some((id, pt, ev) => ()),
  onSelection  = Some(ids => println(s"selected ${ids.size}")),
  enableScroll = true, enableDrag = true, enableCrosshair = true
)
document.body.appendChild(node)   // Canvas or SVG element
update(newPlot)                   // repaint with a different Build
```

## 11. Built-in interactivity (browser)

The Canvas and SVG backends give you, for free: **scroll to zoom**, **drag to
pan**, **shift-drag to box-select**, **hover crosshair** (`enableCrosshair = true`).
Your callbacks receive an `Identifier` telling you what was hit —
`DataRowIdx(externalDataSourceIdx, dataSourceIdx, rowIdx)` points back to the
exact input row, so you can look up the underlying record. Disable interactions
you do not want with `enableScroll = false` / `enableDrag = false`.

Per-point callbacks are **opt-in**: `point()` defaults to `noIdentifier = true`,
which means its marks carry no `Identifier` and never fire `onShapeClick` /
`onHover`. Set `point(noIdentifier = false)` on the renderer to make individual
points hit-testable.

## 12. Gotchas

- **Import a backend.** `import org.nspl._` alone will not render — add
  `awtrenderer._`, `canvasrenderer._`, or `svgrenderer._` for the implicits.
- **Call `.build`.** JVM output helpers want the built plot; a factory returns a
  `Build`. (`render` in the browser takes the `Build` directly.)
- **Sizes are font-relative** (`fts`), not pixels. Output resolution is set by
  the `width` you pass to the renderer.
- **Log axes require strictly positive data** — `xlog(true)`/`ylog(true)` throw
  on a zero or negative value in that dimension.
- **Legends are opt-in.** A plain `data -> renderer` layer is `NotInLegend`; wrap
  it as `(data, renderer, InLegend("name"))`, or add entries via
  `par.extraLegend(...)`, to make it appear.
- **Scala.js dependencies use `%%%`**, not `%%`.
