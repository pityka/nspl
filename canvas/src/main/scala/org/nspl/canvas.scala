package org.nspl

import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.dom.html
import scala.collection.mutable.ArrayBuffer

case class CanvasRC(
    graphics: CanvasRenderingContext2D,
    cick: Identifier => Unit
) extends RenderingContext {
  var mousedown = false
  val plotAreaShapes = ArrayBuffer[(Shape, PlotAreaIdentifier)]()

  def registerPlotArea(shape: Shape, id: PlotAreaIdentifier) =
    plotAreaShapes.append((shape, id))

  def processPlotArea(e: MouseEvent, p: Point)(
      cb: PlotAreaIdentifier => Unit
  ) = {
    hitTest(e, p, plotAreaShapes, cb)
  }

  private def hitTest[T](
      e: MouseEvent,
      p: Point,
      shapes: collection.Seq[(Shape, T)],
      callback: T => Unit
  ) = {
    import canvasrenderer._
    val ctx = graphics

    shapes.foreach { case (shape, id) =>
      val hit = shape match {
        case Rectangle(x, y, w, h, tx, _) =>
          tx.applyTo(ctx)
          ctx.beginPath()
          ctx.rect(x, y, w, h)
          AffineTransform.identity.applyTo(ctx)
          val r = ctx.isPointInPath(p.x, p.y)
          r

        case _ => ???
      }
      if (hit) {
        callback(id)
      }

    }
  }

}

object canvasrenderer {

  implicit val defaultGlyphMeasurer = CanvasGlyphMeasurer

  implicit val defaultAWTFont: FontConfiguration = importFont("Arial")

  implicit def rec2bounds(r: ClientRect) =
    Bounds(r.left, r.top, r.width, r.height)

  def cssColor(c: Color) = s"rgba(${c.r},${c.g},${c.b},${c.a}"

  def getCanvasCoordinate(canvas: html.Canvas, e: MouseEvent): Point = {
    def rect = canvas.getBoundingClientRect()
    val x = e.clientX - rect.left
    val y = e.clientY - rect.top
    Point(x, y)
  }

  def render[K <: Renderable[K]](
      build0: Build[K],
      width: Int,
      height: Int,
      click: Identifier => Unit = (_ => ())
  )(implicit
      er: Renderer[K, CanvasRC]
  ) = {

    val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]

    val ctx =
      CanvasRC(
        canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D],
        click
      )

    var build = build0
    var paintableElem = build.build
    var dragStart = Point(0, 0)
    var queuedCallback: Double => Unit = null

    def queueAnimationFrame(body: Double => Unit) = {

      if (queuedCallback == null) {
        dom.window.requestAnimationFrame { d =>
          val cb = queuedCallback
          queuedCallback = null
          cb(d)
        }
      }
      queuedCallback = body

    }

    def paint() = {
      val aspect = paintableElem.bounds.h / paintableElem.bounds.w
      val height = (width * aspect)

      canvas.height = height.toInt
      canvas.width = width
      ctx.graphics.clearRect(0, 0, canvas.width, canvas.height);
      ctx.plotAreaShapes.clear()
      fitToBounds(paintableElem, Bounds(0, 0, width, height))
        .render(ctx)
    }

    def onmousedown(e: MouseEvent) = {
      if (e.button == 0) {
        e.preventDefault()
        queueAnimationFrame { _ =>
          val componentBounds: Bounds = canvas.getBoundingClientRect()
          val p = getCanvasCoordinate(canvas, e)
          ctx.processPlotArea(e, p) { _ =>
            ctx.mousedown = true
            dragStart = p
          }
        }
      }
    }

    def onmove(e: MouseEvent) = {
      if (e.button == 0 && ctx.mousedown) {
        e.preventDefault()
        queueAnimationFrame { _ =>
          val componentBounds = canvas.getBoundingClientRect()
          val p = getCanvasCoordinate(canvas, e)
          val v = Point(dragStart.x - p.x, dragStart.y - p.y)
          val l = math.sqrt(v.x * v.x + v.y * v.y)
          if (l > 0) {
            ctx.processPlotArea(e, p) { id =>
              paintableElem =
                build(Some(paintableElem) -> Drag(dragStart, p, id))
              dragStart = p
            }
          }
          paint()
        }
      }
    }

    def onwheel(e: MouseEvent) = {
      e.preventDefault()
      queueAnimationFrame { _ =>
        val cb = canvas.getBoundingClientRect()
        val p = getCanvasCoordinate(canvas, e)
        ctx.processPlotArea(e, p) { id =>
          paintableElem = build(
            Some(paintableElem) -> Scroll(
              e.asInstanceOf[scala.scalajs.js.Dynamic]
                .deltaY
                .asInstanceOf[Double],
              p,
              id
            )
          )
        }
        paint()
      }
    }

    def update = (k: Build[K]) => {
      build = k
      paintableElem = k.build
      paint()
    }

    canvas.onmousedown = onmousedown _
    canvas.onmouseup = { _ =>
      ctx.mousedown = false
    }
    canvas.onmousemove = onmove _
    canvas.addEventListener("wheel", onwheel _)
    paint()

    (canvas, update)

  }

  def fill(sh: Shape, ctx: CRC) = sh match {
    case Rectangle(x, y, w, h, tx, _) => {
      tx.applyTo(ctx)
      ctx.fillRect(x, y, w, h)

    }
    case Ellipse(x, y, w, h, tx) => {
      val centerX = x + 0.5 * w
      val centerY = y + 0.5 * h
      val radiusX = w * 0.5
      val radiusY = h * 0.5

      tx.applyTo(ctx)
      ctx.beginPath()
      ctx
        .asInstanceOf[scala.scalajs.js.Dynamic]
        .ellipse(centerX, centerY, radiusX, radiusY, 0, 0, 2 * Math.PI)
      ctx.fill()
    }
    case Line(x1, y1, x2, y2) => ()
    case SimplePath(points) =>
      AffineTransform.identity.applyTo(ctx)
      ctx.beginPath()
      points.foreach { p =>
        ctx.lineTo(p.x, p.y)
      }
      ctx.fill()

    case Path(ops) =>
      AffineTransform.identity.applyTo(ctx)
      ctx.beginPath()
      ops foreach {
        case MoveTo(Point(x, y)) => ctx.moveTo(x, y)
        case LineTo(Point(x, y)) => ctx.lineTo(x, y)
        case QuadTo(Point(x2, y2), Point(x1, y1)) =>
          ctx.quadraticCurveTo(x1, y1, x2, y2)
        case _ => ???
        // case CubicTo(Point(x3, y3), Point(x1, y1), Point(x2, y2)) => path.curveTo(x1, y1, x2, y2, x3, y3)
      }
      ctx.fill()

  }

  def draw(sh: Shape, ctx: CRC) = sh match {
    case Rectangle(x, y, w, h, tx, _) => {
      tx.applyTo(ctx)
      ctx.strokeRect(x, y, w, h)
      // AffineTransform.identity.applyTo(ctx)
    }
    case Ellipse(x, y, w, h, tx) => {
      val centerX = x + 0.5 * w
      val centerY = y + 0.5 * h
      val radiusX = w * 0.5
      val radiusY = h * 0.5

      tx.applyTo(ctx)
      ctx.beginPath()
      ctx
        .asInstanceOf[scala.scalajs.js.Dynamic]
        .ellipse(centerX, centerY, radiusX, radiusY, 0, 0, 2 * Math.PI)
      ctx.stroke()
      // AffineTransform.identity.applyTo(ctx)
    }
    case Line(x1, y1, x2, y2) => {
      AffineTransform.identity.applyTo(ctx)
      ctx.beginPath()
      ctx.moveTo(x1, y1)
      ctx.lineTo(x2, y2)
      ctx.stroke()
    }
    case SimplePath(points) => {
      AffineTransform.identity.applyTo(ctx)
      ctx.beginPath()
      points.foreach { p =>
        ctx.lineTo(p.x, p.y)
      }
      ctx.stroke()
    }
    case Path(ops) => {
      AffineTransform.identity.applyTo(ctx)
      ctx.beginPath()
      ops foreach {
        case MoveTo(Point(x, y)) => ctx.moveTo(x, y)
        case LineTo(Point(x, y)) => ctx.lineTo(x, y)
        case QuadTo(Point(x2, y2), Point(x1, y1)) =>
          ctx.quadraticCurveTo(x1, y1, x2, y2)
        case _ => ???
        // case CubicTo(Point(x3, y3), Point(x1, y1), Point(x2, y2)) => path.curveTo(x1, y1, x2, y2, x3, y3)
      }
      ctx.stroke()
    }
  }

  type CRC = CanvasRenderingContext2D

  implicit class PimpedColor(c: Color) {

    def css = s"rgba(${c.r},${c.g},${c.b})"
  }

  implicit class PimpedTx(tx: AffineTransform) {
    def applyTo(ctx: CRC) = {
      ctx.setTransform(tx.m0, tx.m3, tx.m1, tx.m4, tx.m2, tx.m5)
    }
  }

  implicit class Pimp[K <: Renderable[K]](t: K) {
    def render(ctx: CanvasRC)(implicit r: CER[K]) = r.render(ctx, t)
  }

  type CER[T] = Renderer[T, CanvasRC]

  implicit val shapeRenderer = new CER[ShapeElem] {
    def render(ctx: CanvasRC, elem: ShapeElem): Unit = {
      if (elem.fill.a > 0.0) {
        ctx.graphics.fillStyle = elem.fill.css
        fill(elem.shape, ctx.graphics)
      }
      if (elem.stroke.isDefined && elem.strokeColor.a > 0) {
        ctx.graphics.lineWidth = elem.stroke.get.width * 0.4
        ctx.graphics.strokeStyle = elem.strokeColor.css
        draw(elem.shape, ctx.graphics)
      }

      elem.identifier match {
        case pa: PlotAreaIdentifier =>
          ctx.registerPlotArea(
            elem.shape,
            pa.copy(bounds = Some(elem.bounds))
          )
        case _ =>
      }

    }
  }

  implicit val textRenderer = new CER[TextBox] {

    def render(ctx: CanvasRC, elem: TextBox): Unit = {
      AffineTransform.identity.applyTo(ctx.graphics)
      // ctx.graphics.strokeRect(elem.bounds.x, elem.bounds.y, elem.bounds.w, elem.bounds.h)
      if (!elem.layout.isEmpty) {
        ctx.graphics.fillStyle = elem.color.css
        ctx.graphics.font = canvasFont(elem.font)
        elem.layout.lines.foreach { case (line, lineTx) =>
          val tx = elem.txLoc.concat(lineTx)
          tx.applyTo(ctx.graphics)
          ctx.graphics.fillText(line, 0, 0)
        }
      }
      AffineTransform.identity.applyTo(ctx.graphics)
    }
  }
}
