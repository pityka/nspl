package org.nspl

// Exclude `Event` and `Selection` from the wildcard import — they would
// shadow our `org.nspl.Event` and `org.nspl.Selection` under Scala 3.
import org.scalajs.dom.{Event => _, Selection => _, _}
import org.scalajs.dom
import org.scalajs.dom.html
import scala.collection.mutable.ArrayBuffer
import java.util.ConcurrentModificationException

private[nspl] class RunningAvg {
  private var avg = 0d
  private var n = 0d
  private var n0 = 0L
  def add(t: Double) = {
    if (n0 > 50) {
      n += 1
      avg = avg + (t - avg) / n
    } else {

      n0 += 1
    }
  }
  def current = avg
  def currentN = n
}

/** Rendering context for the HTML5 canvas backend. The optional callbacks
  * are wired up by [[canvasrenderer.render]]; user code constructs instances
  * indirectly through that entry point.
  */
class CanvasRC private[nspl] (
    private[nspl] val graphics: CanvasRenderingContext2D,
    private[nspl] val cick: Identifier => Unit,
    private[nspl] val onHover: Option[(Identifier, MouseEvent) => Unit],
    private[nspl] val onUnhover: Option[(Identifier, MouseEvent) => Unit],
    private[nspl] val onShapeClick: Option[(Identifier, MouseEvent) => Unit],
    private[nspl] val onSelection: Option[collection.Seq[Identifier] => Unit]
) extends RenderingContext[CanvasRC] {

  private[nspl] var transform: AffineTransform = AffineTransform.identity
  private[nspl] var fillColor: Color = Color.black
  private[nspl] var strokeColor: Color = Color.black
  private[nspl] var dash: Seq[Double] = Nil
  private[nspl] var transformInGraphics: AffineTransform =
    AffineTransform.identity
  private[nspl] val bench = new RunningAvg

  private[nspl] def withDash[T](d: Seq[Double])(f: => T) = {
    val current = dash
    if (current != d) {
      dash = d
      graphics.setLineDash(scalajs.js.Array.apply(d: _*))
    }
    f
  }
  private[nspl] def withFill[T](color: Color)(f: => T) = {
    val current = fillColor
    if (current != color) {
      fillColor = color
      graphics.fillStyle = canvasrenderer.asCss(color)
    }
    f
  }
  private[nspl] def withStroke[T](color: Color)(f: => T) = {
    val current = strokeColor
    if (current != color) {
      strokeColor = color
      graphics.strokeStyle = canvasrenderer.asCss(color)
    }
    f
  }

  type LocalTx = AffineTransform

  def localToScala(tx: AffineTransform): AffineTransform = tx

  def concatTransform(tx: AffineTransform): Unit = {
    transform = transform.applyBefore(tx)
  }

  def setTransform(tx: LocalTx): Unit = {
    transform = tx
  }
  def setTransformInGraphics() = {
    if (transformInGraphics != transform) {
      transformInGraphics = transform
      graphics.setTransform(
        transform.m0,
        transform.m3,
        transform.m1,
        transform.m4,
        transform.m2,
        transform.m5
      )
    }
  }

  def getTransform: LocalTx = transform

  private[nspl] var mousedown = false
  private[nspl] var dragShift = false
  private[nspl] val plotAreaShapes = ArrayBuffer[(Shape, PlotAreaIdentifier)]()
  private[nspl] val clickShapes = ArrayBuffer[(Shape, Identifier)]()
  private[nspl] val hoverShapes = ArrayBuffer[(Shape, Identifier)]()
  private[nspl] val selectionShapes = ArrayBuffer[(Shape, Identifier)]()
  private[nspl] var lastHovered: collection.Seq[Int] = Vector.empty

  private[nspl] def registerPlotArea(shape: Shape, id: PlotAreaIdentifier) =
    plotAreaShapes.append((shape, id))

  private[nspl] def registerOnClick(shape: Shape, id: Identifier) =
    clickShapes.append((shape, id))
  private[nspl] def registerOnHover(shape: Shape, id: Identifier) =
    hoverShapes.append((shape, id))
  private[nspl] def registerOnSelection(shape: Shape, id: Identifier) =
    selectionShapes.append((shape, id))

  private[nspl] def processPlotArea(p: Point)(
      cb: PlotAreaIdentifier => Unit
  ) = {
    hitTest[PlotAreaIdentifier](
      p,
      true,
      plotAreaShapes,
      (id, bounds) => cb(id.copy(bounds = bounds))
    )
  }

  /** Hit-test the registered hover shapes at point `p`, invoke `onHover` for
    * each hit, and invoke `onUnhover` for shapes that were hit on the
    * previous call but are not hit this call.
    */
  private[nspl] def callHoverCallbackOnHit(e: MouseEvent, p: Point): Unit =
    onHover.foreach { hover =>
      val previous = lastHovered
      val current = hitTest[Identifier](
        p,
        false,
        hoverShapes,
        (id, _) => hover(id, e)
      )
      lastHovered = current
      val gone = previous.filterNot(current.contains)
      onUnhover.foreach { unhover =>
        gone.foreach { idx =>
          if (idx >= 0 && idx < hoverShapes.size)
            unhover(hoverShapes(idx)._2, e)
        }
      }
    }

  private[nspl] def callClickCallbackOnHit(e: MouseEvent, p: Point): Unit =
    onShapeClick.foreach { click =>
      hitTest[Identifier](
        p,
        false,
        clickShapes,
        (id, _) => click(id, e)
      )
    }

  private[nspl] def callSelectionCallback(p1: Point, p2: Point): Unit =
    onSelection.foreach { sel =>
      val rect = Bounds.fromPoints(p1, p2)
      val hits = selectionShapes
        .filter { case (sh, _) =>
          val b = sh.bounds
          rect.contains(Point(b.centerX, b.centerY))
        }
        .map(_._2)
      sel(hits)
    }

  /** Iterate shapes, hit-test each, fire callback on hits, return the
    * indices of shapes that were hit. Indices are useful so the caller
    * can compare against a previous call's hit set (for unhover).
    */
  private def hitTest[T](
      p: Point,
      needsTransformedBounds: Boolean,
      shapes: collection.Seq[(Shape, T)],
      callback: (T, Option[Bounds]) => Unit
  ): collection.Seq[Int] = {
    val ctx = graphics
    val out = ArrayBuffer.empty[Int]
    var idx = 0
    val n = shapes.size
    while (idx < n) {
      val (shape, id) = shapes(idx)
      val (hit, transformedBounds) = withTransform(shape.currentTransform) {
        setTransformInGraphics()
        val tb =
          if (needsTransformedBounds)
            id match {
              case pl: PlotAreaIdentifier =>
                pl.bounds.map(transform.transform)
              case _ => None
            }
          else None
        val isHit = canvasrenderer.shapeContains(ctx, shape, p)
        (isHit, tb)
      }
      if (hit) {
        callback(id, transformedBounds)
        out.append(idx)
      }
      idx += 1
    }
    out
  }

  def clear() = {
    graphics.setTransform(1, 0, 0, 1, 0, 0)
    transformInGraphics = AffineTransform.identity
    graphics.clearRect(0, 0, graphics.canvas.width, graphics.canvas.height)
    plotAreaShapes.clear()
    clickShapes.clear()
    hoverShapes.clear()
    selectionShapes.clear()
  }

}

object canvasrenderer {

  implicit val defaultGlyphMeasurer: Font.GlyphMeasurer = CanvasGlyphMeasurer

  implicit val defaultFont: FontConfiguration = font("Arial")

  private[nspl] def rec2bounds(r: DOMRect): org.nspl.Bounds =
    Bounds(r.left, r.top, r.width, r.height)

  private[nspl] def cssColor(c: Color) = s"rgba(${c.r},${c.g},${c.b},${c.a}"

  private[nspl] def getCanvasCoordinate(
      canvas: html.Canvas,
      e: MouseEvent,
      devicePixelRatio: Double
  ): Point = {
    def rect = canvas.getBoundingClientRect()
    val x = e.clientX - rect.left
    val y = e.clientY - rect.top
    Point(x * devicePixelRatio, y * devicePixelRatio)
  }

  /** Hit-test using the 2D canvas Path API. Builds the path matching the
    * shape's geometry then calls `isPointInPath`. Supports every Shape
    * subtype — previously only Rectangle was handled and the rest fell
    * into `???`.
    */
  private[nspl] def shapeContains(
      ctx: CanvasRenderingContext2D,
      shape: Shape,
      p: Point
  ): Boolean = shape match {
    case Rectangle(x, y, w, h, _, _) =>
      ctx.beginPath()
      ctx.rect(x, y, w, h)
      ctx.isPointInPath(p.x, p.y)
    case Ellipse(x, y, w, h, _) =>
      val cx = x + 0.5 * w
      val cy = y + 0.5 * h
      val rx = 0.5 * w
      val ry = 0.5 * h
      ctx.beginPath()
      ctx
        .asInstanceOf[scala.scalajs.js.Dynamic]
        .ellipse(cx, cy, rx, ry, 0, 0, 2 * Math.PI)
      ctx.isPointInPath(p.x, p.y)
    case ln: Line =>
      // A 1D line in the path API has zero area; treat as containing the
      // point if it is within a small tolerance of the segment.
      val dx = ln.x2 - ln.x1
      val dy = ln.y2 - ln.y1
      val len2 = dx * dx + dy * dy
      if (len2 == 0d) {
        val ex = p.x - ln.x1
        val ey = p.y - ln.y1
        ex * ex + ey * ey <= 4d
      } else {
        val t =
          ((p.x - ln.x1) * dx + (p.y - ln.y1) * dy) / len2
        val tc = if (t < 0d) 0d else if (t > 1d) 1d else t
        val px = ln.x1 + tc * dx
        val py = ln.y1 + tc * dy
        val ex = p.x - px
        val ey = p.y - py
        ex * ex + ey * ey <= 4d
      }
    case sp: SimplePath =>
      ctx.beginPath()
      sp.ps.foreach(pt => ctx.lineTo(pt.x, pt.y))
      ctx.isPointInPath(p.x, p.y)
    case path: Path =>
      ctx.beginPath()
      path.path.foreach {
        case op: PathOperation.MoveTo => ctx.moveTo(op.p.x, op.p.y)
        case op: PathOperation.LineTo => ctx.lineTo(op.p.x, op.p.y)
        case op: PathOperation.QuadTo =>
          ctx.quadraticCurveTo(op.p1.x, op.p1.y, op.p2.x, op.p2.y)
        case op: PathOperation.CubicTo =>
          ctx.bezierCurveTo(
            op.p1.x,
            op.p1.y,
            op.p2.x,
            op.p2.y,
            op.p3.x,
            op.p3.y
          )
      }
      ctx.isPointInPath(p.x, p.y)
  }

  /** Render a `Build[K]` into a fresh canvas element and return the
    * element plus an updater function. Optional callbacks let user code
    * react to hover/click/selection on shapes that were registered with
    * a non-empty [[Identifier]].
    *
    * @param click
    *   plot-area click callback (fires on mousedown over a plot area).
    *   Preserved for backward compatibility.
    * @param onHover
    *   fires when the mouse moves over any shape carrying a non-empty
    *   identifier.
    * @param onUnhover
    *   fires when a previously-hovered shape is no longer under the mouse.
    * @param onShapeClick
    *   fires on click (mouseup without drag) on any shape with a non-empty
    *   identifier.
    * @param onSelection
    *   fires when the user finishes a shift+drag rectangle, receiving the
    *   identifiers of all shapes whose centers fall in the rectangle.
    * @param enableScroll
    *   if false, wheel events are not consumed.
    * @param enableDrag
    *   if false, plain drags do not produce Drag events. Shift+drag still
    *   produces Selection events when `onSelection` is set.
    */
  def render[K <: Renderable[K]](
      build0: Build[K],
      width: Int,
      height: Int,
      click: Identifier => Unit = (_ => ()),
      onHover: Option[(Identifier, MouseEvent) => Unit] = None,
      onUnhover: Option[(Identifier, MouseEvent) => Unit] = None,
      onShapeClick: Option[(Identifier, MouseEvent) => Unit] = None,
      onSelection: Option[collection.Seq[Identifier] => Unit] = None,
      enableScroll: Boolean = true,
      enableDrag: Boolean = true
  )(implicit
      er: Renderer[K, CanvasRC]
  ): (html.Canvas, Build[K] => Unit) = {

    val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    canvas.style.width = s"${width}px"
    canvas.style.height = s"${height}px"
    val devicePixelRatio = window.devicePixelRatio
    canvas.width = (width * devicePixelRatio).toInt
    canvas.height = (height * devicePixelRatio).toInt

    val ctx =
      new CanvasRC(
        canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D],
        click,
        onHover,
        onUnhover,
        onShapeClick,
        onSelection
      )

    var build = build0
    var paintableElem = build.build
    var dragStart = Point(0, 0)
    var queuedCallback: Double => Unit = null
    val eventStore = new EventFusionHelper

    def paintBounds = {
      // Fit-inside: scale the plot so it fits entirely within the canvas
      // without distorting its aspect. The previous version compared the
      // plot aspect to `1` rather than to the *canvas* aspect, which
      // overflowed the canvas height when the canvas was wider than tall
      // (e.g. 600×200) but the plot was still taller than the canvas in
      // proportion (e.g. plot aspect 0.7 on a canvas aspect 0.33).
      val plotAspect = paintableElem.bounds.h / paintableElem.bounds.w
      val canvasAspect = canvas.height.toDouble / canvas.width.toDouble
      if (plotAspect > canvasAspect) {
        // plot is taller (relative) than canvas → fit to height
        val w = (canvas.height / plotAspect).toInt
        Bounds(0, 0, w, canvas.height)
      } else {
        // plot is wider (relative) than canvas → fit to width
        val h = (canvas.width * plotAspect).toInt
        Bounds(0, 0, canvas.width, h)
      }
    }

    def queueAnimationFrame(body: Double => Unit) = {

      if (queuedCallback == null) {
        dom.window.requestAnimationFrame { d =>
          val cb = queuedCallback
          queuedCallback = null
          try {
            cb(d)
          } catch {
            case _: ConcurrentModificationException => ()
          }
        }
      }
      queuedCallback = body

    }

    def bench[T](enabled: Boolean)(f: => T) = if (enabled) {
      val t1 = System.nanoTime
      val r = f
      val t2 = System.nanoTime
      ctx.bench.add((t2 - t1).toDouble)
      if (ctx.bench.currentN % 100 == 0) {
        println(ctx.bench.current * 1e-9)
      }
      r
    } else f

    def paint() = bench(false) {

      ctx.clear()

      ctx.render(
        fitToBounds(paintableElem, paintBounds)
      )
    }

    def onmousedown(e: MouseEvent) = {
      if (e.button == 0) {
        e.preventDefault()
        val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
        ctx.processPlotArea(p) { identifier =>
          ctx.mousedown = true
          ctx.dragShift = e.shiftKey
          dragStart = p
          click(identifier)
        }
      }
    }

    def onmouseup(e: MouseEvent) = {
      if (e.button == 0 && ctx.mousedown) {
        e.preventDefault()
        val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
        if (ctx.dragShift && onSelection.isDefined) {
          ctx.callSelectionCallback(dragStart, p)
        }
      }
      ctx.mousedown = false
      ctx.dragShift = false
    }

    def onclickHandler(e: MouseEvent) = {
      if (e.button == 0 && onShapeClick.isDefined) {
        val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
        dom.window.requestAnimationFrame { _ =>
          ctx.callClickCallbackOnHit(e, p)
        }
      }
    }

    def onmove(e: MouseEvent) = {
      val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
      // hover: only when no button is pressed
      if (e.buttons == 0 && onHover.isDefined) {
        queueAnimationFrame { _ =>
          ctx.callHoverCallbackOnHit(e, p)
        }
      }
      if (e.button == 0 && ctx.mousedown) {
        e.preventDefault()
        queueAnimationFrame { _ =>
          val v = Point(dragStart.x - p.x, dragStart.y - p.y)
          val l = math.sqrt(v.x * v.x + v.y * v.y)
          if (l > 0) {
            ctx.processPlotArea(p) { id =>
              val ev =
                if (ctx.dragShift && onSelection.isDefined)
                  Some(Selection(dragStart, p, id))
                else if (enableDrag)
                  Some(Drag(dragStart, p, id))
                else None
              ev match {
                case Some(event) =>
                  paintableElem = build(Some(paintableElem) -> event)
                  if (!ctx.dragShift) dragStart = p
                  paint()
                  eventStore.add(event, id.canFuseEvents)
                case None => ()
              }
            }
          }
        }
      }
    }

    def onwheel(e: MouseEvent) = {
      if (enableScroll) {
        e.preventDefault()
        queueAnimationFrame { _ =>
          val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
          ctx.processPlotArea(p) { id =>
            val event = Scroll(
              e.asInstanceOf[scala.scalajs.js.Dynamic]
                .deltaY
                .asInstanceOf[Double],
              p,
              id
            )
            paintableElem = build(Some(paintableElem) -> event)
            paint()
            eventStore.add(event, id.canFuseEvents)
          }
        }
      }
    }

    val update: Build[K] => Unit = (k: Build[K]) =>
      queueAnimationFrame { _ =>
        build = k
        paintableElem = k.build
        // Replay accumulated events so externally-supplied state changes
        // don't drop the user's current zoom/pan.
        eventStore.get.foreach { (event: org.nspl.Event) =>
          paintableElem = build(Some(paintableElem) -> event)
        }
        paint()
      }

    canvas.onmousedown = onmousedown _
    canvas.onmouseup = onmouseup _
    canvas.onmousemove = onmove _
    if (onShapeClick.isDefined) {
      canvas.onclick = onclickHandler _
    }
    canvas.addEventListener("wheel", onwheel _)

    queueAnimationFrame { _ =>
      paint()
    }

    (canvas, update)

  }

  private[nspl] def fill(sh: Shape, graphics: CanvasRenderingContext2D) =
    sh match {
      case sh: Rectangle => {

        graphics.fillRect(sh.x, sh.y, sh.w, sh.h)

      }
      case sh: Ellipse => {
        val centerX = sh.x + 0.5 * sh.w
        val centerY = sh.y + 0.5 * sh.h
        val radiusX = sh.w * 0.5
        val radiusY = sh.h * 0.5

        graphics.beginPath()
        graphics
          .asInstanceOf[scala.scalajs.js.Dynamic]
          .ellipse(centerX, centerY, radiusX, radiusY, 0, 0, 2 * Math.PI)
        graphics.fill()
      }
      case _: Line => ()
      case sh: SimplePath =>
        graphics.beginPath()
        sh.ps.foreach { p =>
          graphics.lineTo(p.x, p.y)
        }
        graphics.fill()

      case sh: Path =>
        graphics.beginPath()
        sh.path foreach {
          case cm: PathOperation.MoveTo => graphics.moveTo(cm.p.x, cm.p.y)
          case cm: PathOperation.LineTo => graphics.lineTo(cm.p.x, cm.p.y)
          case cm: PathOperation.QuadTo =>
            graphics.quadraticCurveTo(cm.p1.x, cm.p1.y, cm.p2.x, cm.p2.y)
          case cm: PathOperation.CubicTo =>
            graphics.bezierCurveTo(
              cm.p1.x,
              cm.p1.y,
              cm.p2.x,
              cm.p2.y,
              cm.p3.x,
              cm.p3.y
            )
        }
        graphics.fill()

    }

  private[nspl] def draw(
      sh: Shape,
      graphics: CanvasRenderingContext2D
  ) = {
    sh match {
      case sh: Rectangle => {
        graphics.strokeRect(sh.x, sh.y, sh.w, sh.h)
      }
      case sh: Ellipse => {
        val centerX = sh.x + 0.5 * sh.w
        val centerY = sh.y + 0.5 * sh.h
        val radiusX = sh.w * 0.5
        val radiusY = sh.h * 0.5

        graphics.beginPath()
        graphics
          .asInstanceOf[scala.scalajs.js.Dynamic]
          .ellipse(centerX, centerY, radiusX, radiusY, 0, 0, 2 * Math.PI)
        graphics.stroke()
      }
      case sh: Line => {
        graphics.beginPath()
        graphics.moveTo(sh.x1, sh.y1)
        graphics.lineTo(sh.x2, sh.y2)
        graphics.stroke()
      }
      case sh: SimplePath => {
        graphics.beginPath()
        sh.ps.foreach { p =>
          graphics.lineTo(p.x, p.y)
        }
        graphics.stroke()
      }
      case sh: Path => {
        graphics.beginPath()
        sh.path foreach {
          case cm: PathOperation.MoveTo => graphics.moveTo(cm.p.x, cm.p.y)
          case cm: PathOperation.LineTo => graphics.lineTo(cm.p.x, cm.p.y)
          case cm: PathOperation.QuadTo =>
            graphics.quadraticCurveTo(cm.p1.x, cm.p1.y, cm.p2.x, cm.p2.y)
          case cm: PathOperation.CubicTo =>
            graphics.bezierCurveTo(
              cm.p1.x,
              cm.p1.y,
              cm.p2.x,
              cm.p2.y,
              cm.p3.x,
              cm.p3.y
            )
        }
        graphics.stroke()
      }
    }
  }

  implicit val shapeRenderer: Renderer[ShapeElem, CanvasRC] =
    new Renderer[ShapeElem, CanvasRC] {

      private def drawAndFill(ctx: CanvasRC, elem: ShapeElem) = {

        if (
          elem.fill.a > 0 || (elem.stroke.isDefined && elem.strokeColor.a > 0)
        ) {
          ctx.setTransformInGraphics()

          val shape = elem.shape

          if (elem.fill.a > 0) {
            ctx.withFill(elem.fill) {
              fill(shape, ctx.graphics)
            }
          }
          if (elem.stroke.isDefined && elem.strokeColor.a > 0) {
            ctx.withStroke(elem.strokeColor) {

              ctx.withDash(elem.stroke.get.dash) {

                if (ctx.graphics.lineWidth != elem.stroke.get.width) {
                  ctx.graphics.lineWidth = elem.stroke.get.width
                }

                draw(shape, ctx.graphics)
              }
            }
          }
        }
      }

      def render(ctx: CanvasRC, elem: ShapeElem): Unit = {
        ctx.withTransform(elem.tx applyBefore elem.shape.currentTransform) {

          drawAndFill(ctx, elem)

          elem.identifier match {
            case EmptyIdentifier => ()
            case pa: PlotAreaIdentifier =>
              ctx.registerPlotArea(
                elem.shape.transform((_, old) =>
                  ctx.getAffineTransform.applyBefore(old)
                ),
                pa.copy(bounds = Some(elem.bounds))
              )
            case other =>
              val resolved = elem.shape.transform((_, old) =>
                ctx.getAffineTransform.applyBefore(old)
              )
              ctx.registerOnClick(resolved, other)
              ctx.registerOnHover(resolved, other)
              ctx.registerOnSelection(resolved, other)
          }
        }

      }
    }

  private[nspl] def asCss(c: Color) = s"rgba(${c.r},${c.g},${c.b}, ${c.a})"

  implicit val textRenderer: Renderer[TextBox, CanvasRC] =
    new Renderer[TextBox, CanvasRC] {

      def render(ctx: CanvasRC, elem: TextBox): Unit = {
        if (!elem.layout.isEmpty) {
          ctx.withTransform(elem.tx) {

            ctx.withFill(elem.color) {
              // Bold and oblique map directly to the canvas font string.
              // Sub/superscript are emulated as a smaller font with a
              // vertical offset; underline is drawn as a line after the
              // glyphs since the 2D canvas API has no underline switch.
              val sub = elem.subScript && !elem.superScript
              val sup = elem.superScript && !elem.subScript
              val scaledFont =
                if (sub || sup)
                  new Font(
                    elem.font.name,
                    math.max(1, (elem.font.size * 0.7).toInt)
                  )
                else elem.font
              ctx.graphics.font =
                canvasFont(scaledFont, bold = elem.bold, italic = elem.oblique)

              val ascent =
                if (sub || sup) elem.font.size.toDouble * 0.7 else 0d
              val yShift =
                if (sup) -ascent * 0.4
                else if (sub) ascent * 0.4
                else 0d

              elem.layout.lines.foreach { case (line, lineTx) =>
                ctx.withTransform(lineTx) {
                  ctx.setTransformInGraphics()
                  ctx.graphics.fillText(line, 0, yShift)
                  if (elem.underline) {
                    val w = ctx.graphics.measureText(line).width
                    // ~10% of font size below the baseline.
                    val uy = yShift + scaledFont.size.toDouble * 0.1
                    ctx.graphics.fillRect(
                      0d,
                      uy,
                      w,
                      math.max(1d, scaledFont.size.toDouble * 0.05)
                    )
                  }
                }
              }
            }

            // Register this text box as a hover/click/selection target if
            // it carries a non-empty identifier. We register the bounding
            // rectangle since `fillText` itself doesn't establish a path.
            elem.identifier match {
              case EmptyIdentifier => ()
              case pa: PlotAreaIdentifier =>
                ctx.registerPlotArea(
                  Shape.rectangle(
                    elem.bounds.x,
                    elem.bounds.y,
                    elem.bounds.w,
                    elem.bounds.h
                  ),
                  pa.copy(bounds = Some(elem.bounds))
                )
              case other =>
                val rect = Shape.rectangle(
                  elem.bounds.x,
                  elem.bounds.y,
                  elem.bounds.w,
                  elem.bounds.h
                )
                ctx.registerOnClick(rect, other)
                ctx.registerOnHover(rect, other)
                ctx.registerOnSelection(rect, other)
            }
          }
        }
      }
    }
}
