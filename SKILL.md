---
name: nspl-development
description: >
  Develop and modify nspl, a 2D scientific plotting library for Scala and
  Scala.js. Covers the scene-graph + Build architecture, the data pipeline,
  the high-level plot API, the AWT/Canvas/SVG backends, the interaction model,
  and the cross-compile / fatal-warnings / binary-compatibility constraints
  that CI enforces. Load this when editing anything under core, awt, canvas,
  svg-js, shared-jvm, shared-js, or saddle, or when adding a renderer, plot
  parameter, backend, data adapter, or interaction.
---

# Working in nspl

nspl describes a plot as an immutable **scene graph** of geometric elements and
then hands that graph to a **backend** that walks it and draws. Plot definition
and rendering are fully decoupled: the same graph renders to PNG/PDF/SVG/EPS on
the JVM (AWT) or to an interactive HTML5 Canvas / inline SVG in the browser
(Scala.js). There are no external dependencies in `core`.

Read `CLAUDE.md` first for the module list and build commands; this file is the
working model and the task recipes.

## The four things you must understand before editing

**1. Everything drawable is a `Renderable[K]`.** The trait is F-bounded
(`trait Renderable[K] { self: K => }`) and immutable. Instances carry a
`bounds: Bounds` and support `transform`, `translate`, `scale`, `rotate`,
`rotateCenter`. The leaf types are `ShapeElem` (a `Shape` + fill/stroke/identifier)
and `TextBox` (a laid-out string). Composites — `ElemList`, `ElemList2`,
`ElemOption`, `ElemEither`, `Elems1..ElemsN`, `DataElem` — hold other renderables
and combine their bounds. `core/src/main/scala/org/nspl/core.scala` and
`elements.scala` define these.

**2. A plot is a state function, not a value: `Build[A] = ((Option[A], Event)) => A`.**
`core/src/main/scala/org/nspl/events.scala`. High-level factories like `xyplot`
return a `Build`. Call `.build` (which feeds `(None, BuildEvent)`) to get the
initial immutable scene graph. Interactive backends re-invoke the same `Build`
with a real `Event` (`Scroll`, `Drag`, `Selection`, `MouseHover`, `MouseLeave`)
and the previous state to produce the *next* scene graph, then repaint. Live
hover decorations such as the crosshair are parameters *inside* the rebuilt
graph, not overlays the backend paints. Any `Renderable` implicitly lifts to a
constant `Build` via `renderable2build`.

**3. Rendering is a typeclass: `Renderer[E, RC <: RenderingContext[RC]]`.** A
backend is a `RenderingContext` implementation plus a set of implicit
`Renderer` instances. Crucially, a backend only has to supply two primitive
renderers — `Renderer[ShapeElem, RC]` and `Renderer[TextBox, RC]`. Every
composite renderer (`ElemList`, `Elems1..N`, `DataElem`, …) is generic over the
context and derives automatically from those two. You get a working backend by
rendering shapes and text; you extend the scene graph by expressing new visuals
as shapes and text.

**4. Three coordinate spaces and font-relative units.**
World (data) → View (axis pixels, `Axis.worldToView` / `viewToWorld`) → Canvas
(device pixels, filled in by the backend at hit-test time). `PlotAreaIdentifier.mouseToWorld`
inverts a canvas point back to data coordinates through the stored axes and
frame bounds. Sizes are `RelFontSize` (`0.08 fts`, `1 fts`), not pixels — one
unit is roughly the width of one letter. A `FontConfiguration` (aliased
`type FC[_]`) is an implicit threaded through almost every method as `[F: FC]`;
the AWT backend provides `implicit val defaultAWTFont`, and tests declare
`implicit val myfont = font("SansSerif")`.

## Build, test, format — and the constraints that bite

Canonical commands (also in `CLAUDE.md`; CI runs the first line):

```bash
sbt -J-Xmx3000m +compile saddle/test versionPolicyCheck
sbt saddle/test        # main integration suite (renders a plot gallery)
sbt awt/test           # AWT rendering + pure-JVM interaction tests
sbt scalafmtAll        # format before finishing
sbt canvas/Test/fastLinkJS   # build the browser canvas demo (manual)
sbt svgJs/Test/fastLinkJS    # build the browser svg demo (manual)
```

Four constraints cause almost every avoidable failure:

- **`-Xfatal-warnings` is on.** Warnings are errors. Scala 2.13 additionally has
  the full `-Ywarn-unused` / `-Xlint` set: an unused import, unused local,
  unused private, unused param, dead code, or a `"$x"` string missing an
  interpolator all fail the build. Keep imports minimal and remove anything you
  stop using. Suppress only deliberately with `@scala.annotation.nowarn`.
- **Everything cross-compiles on Scala 2.13.16 and 3.3.5.** The `+` in
  `+compile` builds both; without it you only build 2.13 and can miss a 3.x
  break. Scala 3 is configured `-no-indent -old-syntax`: use braces and classic
  syntax, never significant-indentation syntax.
- **Binary compatibility is enforced** by sbt-version-policy (early-semver,
  `versionPolicyIntention`). `versionPolicyCheck` fails on a breaking change to
  a public signature. Prefer additive changes with defaults; see the
  `Parameters` recipe below for the established additive pattern.
- **The `canvas` and `svgJs` backends are deliberate mirrors.** Same `render`
  signature, same replay-log behavior. Change one, change the other, and keep
  the shared logic (which lives in `core`) in sync.

Test framework is MUnit. The interaction/event math is pure and lives in `core`
specifically so it is unit-testable on the JVM (`awt/src/test/scala/interaction.test.scala`,
`core/src/test/scala/org/nspl/events.test.scala`) — test it there, not in a
browser. There are no automated DOM tests for the Scala.js backends; verify them
by building the demo and opening `canvas/index.html` / `svg-js/index.html`.

## The data pipeline

`data.DataSource` is an iterator of fixed-width `Row`s (each `Row` is an indexed
sequence of `Double` plus a `String` label) with per-column min/max. It can be
lazy. `DataSourceWithQuantiles` adds quantiles for box plots.
`core/src/main/scala/org/nspl/data/`.

You rarely build a `DataSource` by hand — implicit adapters convert common
shapes (`data/adapters.scala`, generated tuple conversions in `data.template`):

- `Seq[(Double, Double)]`, `Seq[(Double, Double, Double)]`, tuples of `Double`
  → rows, via `dataSourceFromRows` / `productsToRow`.
- `indexed(Seq[Double])` → an (index, value) source.
- Saddle `Vec`, `Series`, `Mat`, `Frame` → sources via
  `import org.nspl.saddle._` (`saddle/src/main/scala/org/nspl/dataAdaptorsSaddle.scala`).

A `DataRenderer` (`datarenderers.scala`) turns one `Row` into scene-graph
elements. **Column indices are the interface**: `point()` reads x=col0, y=col1
and optional color/size/shape/error-bar columns by configurable index, so you
add a fourth column to a source to drive per-point color, etc. Built-in
renderers: `point`, `line`, `lineSegment`, `bar`, `area`, `boxwhisker`,
`polynom`, `abline`.

## The high-level API in one screen

`xyplot` is the main entry (`core/src/main/scala/org/nspl/simpleplots.scala`).
It is curried — variadic `(DataSource, List[DataRenderer], LegendConfig)` layers,
then a `Parameters`:

```scala
import org.nspl._
import org.nspl.awtrenderer._

val data = Seq(0d -> 0d, 1d -> 1d, 2d -> 4d, 3d -> 9d)
val plot = xyplot(data -> point())(par.xlab("x").ylab("y").main("Squares"))

val pngFile = pngToFile(plot.build)
val bytes   = pngToByteArray(plot.build)
val pdfFile = pdfToFile(plot.build, textAsShapes = false)
```

Implicit conversions (`implicits.scala`) let a layer be written many ways:
`data -> point()`, `(data, point(), InLegend("name"))`, `(data, List(r1, r2))`,
a bare `data` (defaults to `point()`), etc. `LegendConfig` is `NotInLegend`
(default) or `InLegend("label")`.

Multiple series with a legend:

```scala
xyplot(
  (modelXs, line(color = Color.red),  InLegend("model")),
  (dataXs,  point(color = Color.blue), InLegend("data"))
)(par.main("Fit").ylog(true))
```

`par` is the shared, immutable `Parameters` config (`Parameters.scala`) with
builder-style copies in two equivalent naming styles: `par.xlab("x").ylog(true)`
and `par.withXLab("x").withYLog(true)`. It governs labels, limits (`xlim`/`ylim`),
log axes, ticks, grid, padding, legend, fonts, rotation, crosshair mode, and
more.

Other factories in the same file: `xyzplot` (experimental 3D mesh), `boxplot`,
`binnedboxplot`, `contourplot`, `rasterplot` (bitmap/heatmap), `stackedBarPlot`.
Saddle sugar: `barplotVertical`, `barplotHorizontal`, `rasterplotFromFrame`.

## Composition and layout

Combine finished renderables (not just data series) into figures:

- `group(a, b, …, layout)` composes a small fixed number of renderables into an
  `Elems{N}` (generated from `core/src/main/boilerplate/composite.template`).
  `zgroup` controls z-order.
- `sequence(Seq[Renderable], layout)` → `ElemList`; `sequence2` for a `Seq` of
  `Either` → `ElemList2`.
- Layouts (`layouts.scala`): `TableLayout(columns)`, `ColumnLayout(rows)`,
  `VerticalStack`, `HorizontalStack`, `ZStack`, `FreeLayout`, `RelativeToFirst`.
  `Align` (`align.scala`) has corner/center/anchor helpers.
- `fitToBounds`, `fitToWidth`, `fitToHeight` rescale a renderable, preserving
  aspect ratio.

```scala
group(plotA, plotB, plotC, TableLayout(2))
```

## Colors and axes

`color.scala`: `Color(r,g,b,a)` (also named constants), and `Colormap`s —
`HeatMapColors`, `LogHeatMapColors`, `GrayScale`, `RedBlue`, `DiscreteColors(n)`,
`TableColormap`, `ManualColor`. A `Colormap` maps a `Double` to a `Color`;
`.withRange(min,max)` rescales it. `NaN` conventionally maps to transparent.

`axis.scala`: `AxisFactory` implementations `LinearAxisFactory`,
`Log10AxisFactory`, `Log2AxisFactory`; `AxisSettings` bundles ticks, width,
label rotation, formatter. Log axes throw on non-positive input by design —
that is why a bar on a log y-axis was a real bug (`render.test.scala` guards it).

## Backends and output

**JVM / AWT** — `import org.nspl.awtrenderer._` (`awt/`). Output helpers
(`awtutil.scala`): `pngToFile`, `pdfToFile`, `svgToFile`, `renderToFile`,
`renderToByteArray`, `pngToByteArray`, `pdfToByteArray`, `svgToByteArray`, and
`show(build)` for a live Swing window. Vector formats go through VectorGraphics2D;
`textAsShapes` chooses between real glyphs and outlined shapes. All take a
`Build[K]` (a `Renderable` converts implicitly) and need an implicit
`Renderer[K, JavaRC]` in scope, which the `awtrenderer` import supplies.

**Scala.js Canvas / SVG** — `import canvasrenderer._` or `import svgrenderer._`
(`canvas/`, `svg-js/`). Both expose the same interactive entry point:

```scala
val (node, update) = render(
  plot,                         // a Build[K]
  width = 600, height = 400,
  onShapeClick = Some((id, pt, ev) => ...),
  onHover      = Some((id, pt, ev) => ...),
  onUnhover    = Some((id, pt, ev) => ...),
  onSelection  = Some(ids => ...),
  enableScroll = true, enableDrag = true, enableCrosshair = true
)
document.body.appendChild(node)   // node is a Canvas or SVGSVGElement
update(nextBuild)                 // push a new Build to repaint
```

The two differ only where they must: the SVG backend hit-tests with pure math
(no `isPointInPath`), is resolution-independent (`viewBox`, no
`devicePixelRatio`), and sets `pointer-events="all"` so the empty plot interior
still fires events.

## Interaction model

`core/src/main/scala/org/nspl/events.scala` and `plot.scala`. A backend
hit-tests the cursor, constructs an `Event` carrying a `PlotAreaIdentifier`
(with the canvas-space `bounds` filled in), feeds `(Some(previousState), event)`
to the plot's `Build` (`xyplotareaBuild`), gets a fresh `XYPlotArea`, and
repaints. `EventFusionHelper` collapses high-frequency streams (consecutive
Drags, a growing Selection, repeated Hovers) in the replay log so it stays
compact. Scene elements are tagged with an `Identifier` for hit-testing:
`DataRowIdx` (which dataset/row), `TextBoxIdentifier` (a labelled, clickable
text box), `PlotAreaIdentifier` (the plot area, carrying its axes and view
frame). Selection zooms to the selected world rectangle; Scroll zooms about the
cursor; Drag pans. The mapping/zoom math is covered by
`awt/src/test/scala/interaction.test.scala`.

## Recipes

**Add a data renderer** (e.g. a new mark). Implement the `DataRenderer` trait in
`datarenderers.scala`: `render(row, ctx, ...)`, `asLegend`, `xMinMax(ds)`,
`yMinMax(ds)`, `clear`. Read the columns you need positionally from the `Row`,
build `ShapeElem`/`TextBox` values, and emit them with `ctx.render(elem)`. Add a
factory method to the `Renderers` trait mirroring `point`/`line`/`bar` (defaults,
`[F: FC]`, configurable column indices and colormap). Tag emitted shapes with a
`DataRowIdx` when they should be interactive. Add a smoke/render test to
`awt/src/test/scala/render.test.scala`.

**Add a `Parameters` field** (the additive, binary-compatible pattern; follow
how `crosshairMode` and `plotLegendLayout` were added). In `Parameters.scala`:
add the field to the `class Parameters` constructor, add it with a default to
the private companion `apply()`, and add both a `field(v)` and a `withField(v)`
builder returning `copy(...)`. If it affects rendering, thread it from `xyplot`
into `xyplotareaBuild` (`plot.scala`). Never reorder or drop existing
constructor params — that breaks binary compatibility.

**Add a backend element renderer.** Provide `implicit val fooRenderer:
Renderer[FooElem, JavaRC]` (and the `SvgRC` / `CanvasRC` equivalents) in the
respective backend object. If `FooElem` is a composite of shapes and text you
usually do not need this — express it with `ShapeElem`/`TextBox` and the generic
composite renderers cover it.

**Add a whole backend.** Implement `RenderingContext[YourRC]` (transform stack:
`concatTransform`, `getTransform`, `setTransform`, `localToScala`) and just two
renderers, `Renderer[ShapeElem, YourRC]` and `Renderer[TextBox, YourRC]`.
Composite/data renderers derive automatically. Use `awt.scala` as the compact
reference.

**Add or change an interaction.** Add an `Event` case (or handling) in
`events.scala`, handle it in the `Build` in `plot.scala` (`xyplotareaBuild`),
and — because the backends mirror each other — wire hit-testing and dispatch in
*both* `canvas/src/main/scala/org/nspl/canvas.scala` and
`svg-js/src/main/scala/org/nspl/svg.scala`. Keep pure logic (fusion, coordinate
inversion) in `core` and cover it in the JVM interaction tests.

**Add a data adapter.** Put a pure conversion in `core` (`data/adapters.scala`)
or a Saddle-typed one in `saddle` (`dataAdaptorsSaddle.scala`), returning a
`DataSource` (or `DataSourceWithQuantiles` if it must support box plots).

## File index

| Concern | File |
| --- | --- |
| Core abstractions (Renderable, RenderingContext, Renderer, Bounds, Point, identifiers) | `core/src/main/scala/org/nspl/core.scala` |
| Scene-graph composites (ElemList, ElemEither, ShapeElem, TextBox) | `core/src/main/scala/org/nspl/elements.scala` |
| `group` / `Elems{N}` / tuple→Row generators | `core/src/main/boilerplate/composite.template`, `data.template` |
| High-level plot factories | `core/src/main/scala/org/nspl/simpleplots.scala` |
| Plot engine + `xyplotareaBuild` + legends | `core/src/main/scala/org/nspl/plot.scala` |
| Data renderers | `core/src/main/scala/org/nspl/datarenderers.scala` |
| DataSource / Row / adapters | `core/src/main/scala/org/nspl/data/` |
| Config | `core/src/main/scala/org/nspl/Parameters.scala` |
| Events, `Build`, fusion, crosshair mode | `core/src/main/scala/org/nspl/events.scala` |
| Axes / ticks | `core/src/main/scala/org/nspl/axis.scala`, `ticks.scala` |
| Colors | `core/src/main/scala/org/nspl/color.scala` |
| Layout / alignment | `core/src/main/scala/org/nspl/layouts.scala`, `align.scala` |
| Package object (DSL entry, `par`, `sequence`, `fitToBounds`) | `core/src/main/scala/org/nspl/package.scala` |
| AWT backend + output | `awt/src/main/scala/org/nspl/awt.scala`, `awtutil.scala` |
| Canvas backend | `canvas/src/main/scala/org/nspl/canvas.scala` |
| SVG (Scala.js) backend | `svg-js/src/main/scala/org/nspl/svg.scala` |
| Saddle integration | `saddle/src/main/scala/org/nspl/dataAdaptorsSaddle.scala` |
| Usage gallery (best example corpus) | `saddle/src/test/scala/plots/plots.test.scala` |
| Interaction/event tests (pure JVM) | `awt/src/test/scala/interaction.test.scala`, `core/src/test/scala/org/nspl/events.test.scala` |
| Manual JS demos | `canvas/src/test/scala/test.scala` + `canvas/index.html`, `svg-js/src/test/scala/test.scala` + `svg-js/index.html` |
