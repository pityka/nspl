package org.nspl

import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.dom.html

case class CanvasRC(graphics: CanvasRenderingContext2D) extends RenderingContext

object canvasrenderer {

  implicit val defaultGlyphMeasurer = CanvasGlyphMeasurer

  implicit val defaultAWTFont: FontConfiguration = importFont("Arial")

  implicit def rec2bounds(r: ClientRect) =
    Bounds(r.left, r.top, r.width, r.height)

  def render[K <: Renderable[K]](
      elem: Build[K],
      canvas: html.Canvas,
      width: Int
  )(
      implicit er: Renderer[K, CanvasRC]
  ): Unit = {

    val ctx =
      canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    var paintableElem = elem.build
    var dragStart = Point(0, 0)
    var mousedown = false

    def paint = {
      val aspect = paintableElem.bounds.h / paintableElem.bounds.w
      val height = (width * aspect)

      canvas.height = height.toInt
      canvas.width = width
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      fitToBounds(paintableElem, Bounds(0, 0, width, height))
        .render(CanvasRC(ctx))
    }

    def getCanvasCoordinate(e: MouseEvent): Point = {
      val rect = canvas.getBoundingClientRect
      val x = e.clientX - rect.left;
      val y = e.clientY - rect.top;
      Point(x, y)
    }

    def onmousedown(e: MouseEvent) = {
      if (e.button == 0) {
        val componentBounds: Bounds = canvas.getBoundingClientRect
        val p = getCanvasCoordinate(e)
        dragStart = mapPoint(p, componentBounds, paintableElem.bounds)
        mousedown = true
      }
    }

    def makeEvent(e: MouseEvent, up: Boolean) = {
      if (e.button == 0 && mousedown) {
        val componentBounds: Bounds = canvas.getBoundingClientRect
        val p = mapPoint(getCanvasCoordinate(e),
                         componentBounds,
                         paintableElem.bounds)

        val v = Point(dragStart.x - p.x, dragStart.y - p.y)
        val l = math.sqrt(v.x * v.x + v.y * v.y)
        if (l > 0) {
          paintableElem = elem(Some(paintableElem) -> Drag(dragStart, p))
          dragStart = p
        } else {
          if (up) {
            paintableElem = elem(Some(paintableElem) -> Click(p))
          }
        }
        if (up) {
          mousedown = false
        }
        paint
      }
    }

    canvas.onmousedown = onmousedown _
    canvas.onmouseup = makeEvent((_: MouseEvent), true)
    canvas.onmousemove = makeEvent((_: MouseEvent), false)

    paint

  }

  def pathEllipse(x: Double,
                  y: Double,
                  width: Double,
                  height: Double,
                  ctx: CRC) = {

    val centerX = x + .5 * width
    val centerY = y + .5 * height
    val radiusX = width * .5
    val radiusY = height * .5
    var i = 0.0
    while (i <= 2 * math.Pi) {
      val xPos = centerX - (radiusX * math.sin(i)) * Math.sin(0.5 * math.Pi) + (radiusY * math
        .cos(i)) * Math.cos(0.5 * math.Pi);
      val yPos = centerY + (radiusY * math.cos(i)) * math.sin(0.5 * math.Pi) + (radiusX * Math
        .sin(i)) * Math.cos(0.5 * math.Pi);
      if (i == 0) {
        ctx.moveTo(xPos, yPos);
      } else {
        ctx.lineTo(xPos, yPos);
      }
      i += 0.01
    }
    ctx.closePath

  }

  def fill(sh: Shape, ctx: CRC) = sh match {
    case Rectangle(x, y, w, h, tx) => {
      tx.applyTo(ctx)
      ctx.fillRect(x, y, w, h)
    }
    case Ellipse(x, y, w, h, tx) => {
      tx.applyTo(ctx)
      ctx.beginPath
      pathEllipse(x, y, w, h, ctx)
      ctx.fill()
      ctx.closePath
    }
    case Line(x1, y1, x2, y2) => ()
    case SimplePath(points) => {
      ctx.beginPath
      points.foreach { p =>
        ctx.lineTo(p.x, p.y)
      }
      ctx.fill()
      ctx.closePath
    }
    case Path(ops) => {
      ctx.beginPath
      ops foreach {
        case MoveTo(Point(x, y)) => ctx.moveTo(x, y)
        case LineTo(Point(x, y)) => ctx.lineTo(x, y)
        case QuadTo(Point(x2, y2), Point(x1, y1)) =>
          ctx.quadraticCurveTo(x1, y1, x2, y2)
        // case CubicTo(Point(x3, y3), Point(x1, y1), Point(x2, y2)) => path.curveTo(x1, y1, x2, y2, x3, y3)
      }
      ctx.fill()
      ctx.closePath
    }
  }

  def draw(sh: Shape, ctx: CRC) = sh match {
    case Rectangle(x, y, w, h, tx) => {
      tx.applyTo(ctx)
      ctx.strokeRect(x, y, w, h)
    }
    case Ellipse(x, y, w, h, tx) => {
      tx.applyTo(ctx)
      ctx.beginPath
      pathEllipse(x, y, w, h, ctx)
      ctx.stroke()
      ctx.closePath
    }
    case Line(x1, y1, x2, y2) => {
      ctx.beginPath
      ctx.moveTo(x1, y1)
      ctx.lineTo(x2, y2)
      ctx.stroke()
      ctx.closePath
    }
    case SimplePath(points) => {
      ctx.beginPath
      points.foreach { p =>
        ctx.lineTo(p.x, p.y)
      }
      ctx.stroke()
      ctx.closePath
    }
    case Path(ops) => {
      ctx.beginPath
      ops foreach {
        case MoveTo(Point(x, y)) => ctx.moveTo(x, y)
        case LineTo(Point(x, y)) => ctx.lineTo(x, y)
        case QuadTo(Point(x2, y2), Point(x1, y1)) =>
          ctx.quadraticCurveTo(x1, y1, x2, y2)
        // case CubicTo(Point(x3, y3), Point(x1, y1), Point(x2, y2)) => path.curveTo(x1, y1, x2, y2, x3, y3)
      }
      ctx.stroke()
      ctx.closePath
    }
  }

  type CRC = CanvasRenderingContext2D

  implicit class PimpedColor(c: Color) {

    def css = f"#${c.r}%02x${c.g}%02x${c.b}%02x"
  }

  implicit class PimpedTx(tx: AffineTransform) {
    def applyTo(ctx: CRC) = {
      ctx.setTransform(tx.m0, tx.m3, tx.m1, tx.m4, tx.m2, tx.m5)
    }
  }

  def savePaint[T](g: CRC)(fun: CRC => T) = {
    val save = g.fillStyle
    try { fun(g) } finally { if (g.fillStyle != save) { g.fillStyle = save } }
  }

  def saveStroke[T](g: CRC)(fun: CRC => T) = {
    val save1 = g.lineWidth
    val save2 = g.strokeStyle
    try { fun(g) } finally {
      if (g.lineWidth != save1) { g.lineWidth = save1 };
      if (g.strokeStyle != save2) { g.strokeStyle = save2 }
    }
  }

  implicit class Pimp[K <: Renderable[K]](t: K) {
    def render(ctx: CanvasRC)(implicit r: CER[K]) = r.render(ctx, t)
  }

  type CER[T] = Renderer[T, CanvasRC]

  implicit val shapeRenderer = new CER[ShapeElem] {
    def render(ctx: CanvasRC, elem: ShapeElem): Unit = {
      AffineTransform.identity.applyTo(ctx.graphics)
      savePaint(ctx.graphics) { graphics =>
        saveStroke(graphics) { graphics2 =>
          if (elem.fill.a > 0.0) {
            graphics2.fillStyle = elem.fill.css
            fill(elem.shape, graphics2)
          }
          if (elem.stroke.isDefined) {
            graphics2.lineWidth = elem.stroke.get.width
            graphics2.strokeStyle = elem.strokeColor.css
            draw(elem.shape, graphics2)
          }
        }
      }
    }
  }

  implicit val textRenderer = new CER[TextBox] {

    def render(ctx: CanvasRC, elem: TextBox): Unit = {
      AffineTransform.identity.applyTo(ctx.graphics)
      // ctx.graphics.strokeRect(elem.bounds.x, elem.bounds.y, elem.bounds.w, elem.bounds.h)
      if (!elem.layout.isEmpty) {
        savePaint(ctx.graphics) { graphics =>
          saveStroke(graphics) { graphics2 =>
            graphics2.fillStyle = elem.color.css
            graphics2.font = canvasFont(elem.font)
            elem.layout.lines.foreach {
              case (line, lineTx) =>
                val tx = elem.txLoc.concat(lineTx)
                tx.applyTo(graphics2)
                graphics2.fillText(line, 0, 0)
            }
          }
        }
      }
      AffineTransform.identity.applyTo(ctx.graphics)
    }
  }
}
