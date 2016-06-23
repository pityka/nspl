package org.nspl

import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.dom.html

case class CanvasRC(graphics: CanvasRenderingContext2D) extends RenderingContext

object canvasrenderer {

  def render[K <: Renderable[K]](
    elem: K,
    canvas: html.Canvas,
    width: Int
  )(
    implicit
    er: Renderer[K, CanvasRC]
  ): Unit = {

    val ctx =
      canvas.getContext("2d")
        .asInstanceOf[dom.CanvasRenderingContext2D]

    val aspect = elem.bounds.h / elem.bounds.w
    val height = (width * aspect).toInt

    canvas.height = height
    canvas.width = width

    fitToBounds(elem, Bounds(0, 0, width, height)).render(CanvasRC(ctx))

  }

  def pathEllipse(x: Double, y: Double, width: Double, height: Double, ctx: CRC) = {

    val centerX = x + .5 * width
    val centerY = y + .5 * height
    val radiusX = width * .5
    val radiusY = height * .5
    var i = 0.0
    while (i <= 2 * math.Pi) {
      val xPos = centerX - (radiusX * math.sin(i)) * Math.sin(0.5 * math.Pi) + (radiusY * math.cos(i)) * Math.cos(0.5 * math.Pi);
      val yPos = centerY + (radiusY * math.cos(i)) * math.sin(0.5 * math.Pi) + (radiusX * Math.sin(i)) * Math.cos(0.5 * math.Pi);
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
        case QuadTo(Point(x2, y2), Point(x1, y1)) => ctx.quadraticCurveTo(x1, y1, x2, y2)
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
        case QuadTo(Point(x2, y2), Point(x1, y1)) => ctx.quadraticCurveTo(x1, y1, x2, y2)
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
    try { fun(g) } finally { g.fillStyle = save }
  }

  def saveStroke[T](g: CRC)(fun: CRC => T) = {
    val save1 = g.lineWidth
    val save2 = g.strokeStyle
    try { fun(g) } finally { g.lineWidth = save1; g.strokeStyle = save2 }
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

    def wrapText(ctx: CRC, text: String, x: Double, y: Double, maxWidth: Double, lineHeight: Double) = {
      val words = text.split(" ");
      var line = "";
      var y1 = y

      words.zipWithIndex.foreach {
        case (word, n) =>
          val testLine = line + word + " ";
          val metrics = ctx.measureText(testLine)
          val testWidth = metrics.width
          if (testWidth > maxWidth && n > 0) {
            ctx.fillText(line, x, y1 + lineHeight)
            line = word + " "
            y1 += lineHeight
          } else {
            line = testLine
          }
      }
      ctx.fillText(line, x, y1 + lineHeight, maxWidth)
    }

    def render(ctx: CanvasRC, elem: TextBox): Unit = {
      AffineTransform.identity.applyTo(ctx.graphics)
      elem.tx.applyTo(ctx.graphics)

      if (elem.text.size > 0) {
        savePaint(ctx.graphics) { graphics =>
          saveStroke(graphics) { graphics2 =>
            graphics2.fillStyle = elem.color.css
            graphics2.font = s"${elem.fontSize.toInt}pt monospace"
            wrapText(graphics2, elem.text, elem.loc.x, elem.loc.y, elem.width, elem.fontSize)
          }
        }
      }
    }
  }
}
