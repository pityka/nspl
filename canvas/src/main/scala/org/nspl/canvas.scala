package org.nspl

import org.scalajs.dom._
import org.scalajs.dom
import org.scalajs.dom.html
import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

private[nspl] case class CanvasRC(
    graphics: CanvasRenderingContext2D,
    cick: Identifier => Unit
) extends RenderingContext[CanvasRC] {

  var transform: AffineTransform = AffineTransform.identity
  var fillColor: Color = Color.black
  var strokeColor: Color = Color.black
  var dash: Seq[Double] = Nil
  var transformInGraphics: AffineTransform = AffineTransform.identity

  def withDash[T](d: Seq[Double])(f: => T) = {
    val current = dash
    if (current != d) {
      dash = d
      graphics.setLineDash(scalajs.js.Array.apply(d: _*))
    }
    f
  }
  def withFill[T](color: Color)(f: => T) = {
    val current = fillColor
    if (current != color) {
      fillColor = color
      graphics.fillStyle = canvasrenderer.asCss(color)
    }
    f
  }
  def withStroke[T](color: Color)(f: => T) = {
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
    transform = transform.concat(tx)
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

  var mousedown = false
  val plotAreaShapes = ArrayBuffer[(Shape, PlotAreaIdentifier)]()

  def registerPlotArea(shape: Shape, id: PlotAreaIdentifier) =
    plotAreaShapes.append((shape, id))

  def processPlotArea(e: MouseEvent, p: Point)(
      cb: PlotAreaIdentifier => Unit
  ) = {
    hitTest[PlotAreaIdentifier](
      e,
      p,
      true,
      plotAreaShapes,
      (id, bounds) => cb(id.copy(bounds = bounds))
    )
  }

  private def hitTest[T](
      e: MouseEvent,
      p: Point,
      needsTransformedBounds: Boolean,
      shapes: collection.Seq[(Shape, T)],
      callback: (T, Option[Bounds]) => Unit
  ) = {
    import canvasrenderer._
    val ctx = graphics

    shapes.foreach { case (shape, id) =>
      val (hit, transformedBounds) = withTransform(shape.currentTransform) {
        setTransformInGraphics()
        shape match {
          case Rectangle(x, y, w, h, tx, _) =>
            ctx.beginPath()
            ctx.rect(x, y, w, h)
            val r =
              ctx.isPointInPath(p.x, p.y)

            val transformedBounds =
              if (needsTransformedBounds)
                id match {
                  case pl: PlotAreaIdentifier =>
                    pl.bounds.map(transform.transform)
                  case _ => None
                }
              else None
            r -> transformedBounds

          case _ => ???
        }

      }

      if (hit) {
        callback(id, transformedBounds)
      }

    }
  }

  def clear() = {
    graphics.setTransform(1, 0, 0, 1, 0, 0)
    transformInGraphics = AffineTransform.identity
    graphics.clearRect(0, 0, graphics.canvas.width, graphics.canvas.height)
  }

}

object canvasrenderer {

  implicit val defaultGlyphMeasurer = CanvasGlyphMeasurer

  implicit val defaultFont: FontConfiguration = font("Arial")

  private[nspl] def rec2bounds(r: DOMRect) =
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

  def render[K <: Renderable[K]](
      build0: Build[K],
      width: Int,
      height: Int,
      click: Identifier => Unit = (_ => ())
  )(implicit
      er: Renderer[K, CanvasRC]
  ) = {

    val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    canvas.style.width = s"${width}px"
    canvas.style.height = s"${height}px"
    val devicePixelRatio = window.devicePixelRatio
    canvas.width = (width * devicePixelRatio).toInt
    canvas.height = (height * devicePixelRatio).toInt

    val ctx =
      CanvasRC(
        canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D],
        click
      )

    var build = build0
    var paintableElem = build.build
    var dragStart = Point(0, 0)
    var queuedCallback: Double => Unit = null

    def paintBounds = {
      val aspect = paintableElem.bounds.h / paintableElem.bounds.w

      val paintWidth = if (aspect > 1) canvas.height / aspect else canvas.width
      val paintHeight =
        if (aspect <= 1) canvas.width * aspect else canvas.height
      Bounds(0, 0, paintWidth, paintHeight)
    }

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

      ctx.clear()

      ctx.plotAreaShapes.clear()
      ctx.render(
        fitToBounds(paintableElem, paintBounds)
      )
    }

    def onmousedown(e: MouseEvent) = {
      if (e.button == 0) {
        e.preventDefault()
        val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
        ctx.processPlotArea(e, p) { identifier =>
          ctx.mousedown = true
          dragStart = p
          click(identifier)

        }
      }
    }

    def onmove(e: MouseEvent) = {
      if (e.button == 0 && ctx.mousedown) {
        e.preventDefault()
        queueAnimationFrame { _ =>
          val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
          val v = Point(dragStart.x - p.x, dragStart.y - p.y)
          val l = math.sqrt(v.x * v.x + v.y * v.y)
          if (l > 0) {
            ctx.processPlotArea(e, p) { id =>
              paintableElem =
                build(Some(paintableElem) -> Drag(dragStart, p, id))
              dragStart = p
              paint()
            }
          }
        }
      }
    }

    def onwheel(e: MouseEvent) = {
      e.preventDefault()
      queueAnimationFrame { _ =>
        val cb = canvas.getBoundingClientRect()
        val p = getCanvasCoordinate(canvas, e, devicePixelRatio)
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
          paint()
        }
      }
    }

    def update = (k: Build[K]) =>
      queueAnimationFrame { _ =>
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
      case sh: Line => ()
      case sh: SimplePath =>
        graphics.beginPath()
        sh.ps.foreach { p =>
          graphics.lineTo(p.x, p.y)
        }
        graphics.fill()

      case sh: Path =>
        graphics.beginPath()
        sh.path foreach {
          case cm: MoveTo => graphics.moveTo(cm.p.x, cm.p.y)
          case cm: LineTo => graphics.lineTo(cm.p.x, cm.p.y)
          case cm: QuadTo =>
            graphics.quadraticCurveTo(cm.p1.x, cm.p1.y, cm.p2.x, cm.p2.y)
          case cm: CubicTo =>
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
      graphics: CanvasRenderingContext2D,
      stroke: Stroke
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
          case cm: MoveTo => graphics.moveTo(cm.p.x, cm.p.y)
          case cm: LineTo => graphics.lineTo(cm.p.x, cm.p.y)
          case cm: QuadTo =>
            graphics.quadraticCurveTo(cm.p1.x, cm.p1.y, cm.p2.x, cm.p2.y)
          case cm: CubicTo =>
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

  implicit val shapeRenderer = new Renderer[ShapeElem, CanvasRC] {

    private def drawAndFill(ctx: CanvasRC, elem: ShapeElem) = {

      if (
        elem.fill.a > 0d || (elem.stroke.isDefined && elem.strokeColor.a > 0)
      ) {
        ctx.setTransformInGraphics()

        val shape = elem.shape

        if (elem.fill.a > 0.0) {
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

              draw(shape, ctx.graphics, elem.stroke.get)
            }
          }
        }
      }
    }

    def render(ctx: CanvasRC, elem: ShapeElem): Unit = {
      ctx.withTransform(elem.tx concat elem.shape.currentTransform) {

        drawAndFill(ctx, elem)

        elem.identifier match {
          case pa: PlotAreaIdentifier =>
            ctx.registerPlotArea(
              elem.shape.transform(_ => ctx.getAffineTransform),
              pa.copy(bounds = Some(elem.bounds))
            )
          case _ =>
        }
      }

    }
  }

  def asCss(c: Color) = s"rgba(${c.r},${c.g},${c.b}, ${c.a})"

  implicit val textRenderer = new Renderer[TextBox, CanvasRC] {

    def render(ctx: CanvasRC, elem: TextBox): Unit = {
      ctx.withTransform(elem.txLoc) {

        if (!elem.layout.isEmpty) {
          ctx.withFill(elem.color) {
            ctx.graphics.font = canvasFont(elem.font)
            elem.layout.lines.foreach { case (line, lineTx) =>
              ctx.withTransform(lineTx) {
                // ctx.graphics.strokeStyle = asCss(elem.color)
                // ctx.graphics.strokeRect(elem.bounds.x, elem.bounds.y, elem.bounds.w, elem.bounds.h)
                ctx.setTransformInGraphics()
                ctx.graphics.fillText(line, 0, 0)
              }
            }
          }
        }
      }
    }
  }
}
