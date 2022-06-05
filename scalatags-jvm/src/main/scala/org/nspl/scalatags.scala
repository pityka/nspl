package org.nspl

import scalatags.Text.all._
import scalatags.Text.svgTags._
import scalatags.Text.svgAttrs._
import scalatags.Text.svgAttrs
import scalatags.Text.svgTags

private[nspl] case class ScalaTagRC(
    elems: scala.collection.mutable.ArrayBuffer[Modifier]
) extends RenderingContext[ScalaTagRC] {
  var transform: AffineTransform = AffineTransform.identity

  def getTransform: AffineTransform = transform

  type LocalTx = AffineTransform

  def localToScala(tx: AffineTransform): AffineTransform = tx

  def concatTransform(tx: AffineTransform): Unit = {
    transform = transform.applyBefore(tx)
  }

  def setTransform(tx: LocalTx): Unit = {
    transform = tx
  }
}

object scalatagrenderer {

  implicit val defaultGlyphMeasurer = AwtGlyphMeasurer

  implicit val defaultFont: FontConfiguration = org.nspl.font("Arial")

  private[nspl] implicit class PimpedColor(c: Color) {
    def css = f"#${c.r}%02x${c.g}%02x${c.b}%02x"
  }

  private[nspl] implicit class PimpedTx(tx: AffineTransform) {
    def svg = {
      s"matrix(${tx.m0}, ${tx.m3}, ${tx.m1}, ${tx.m4}, ${tx.m2}, ${tx.m5})"
    }
  }

  def svgToFile[K <: Renderable[K]](
      elem: K,
      width: Int = 1000
  )(implicit
      er: Renderer[K, ScalaTagRC]
  ): java.io.File = {
    import java.io._
    val f = File.createTempFile("nspl", ".svg")

    svgToFile(f, elem, width)
  }

  def svgToFile[K <: Renderable[K]](
      f: java.io.File,
      elem: K,
      width: Int
  )(implicit
      er: Renderer[K, ScalaTagRC]
  ): java.io.File = {
    import java.io._
    val os = new FileOutputStream(f)
    val str: String = renderToScalaTag(elem, width).render
    os.write(
      """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
""".getBytes
    )
    os.write(str.getBytes)
    os.close
    f
  }

  def renderToScalaTag[K <: Renderable[K]](
      elem: K,
      width: Int = 1000
  )(implicit
      er: Renderer[K, ScalaTagRC]
  ) = {

    val ctx = ScalaTagRC(scala.collection.mutable.ArrayBuffer[Modifier]())

    val aspect = elem.bounds.h / elem.bounds.w
    val height = (width * aspect).toInt

    ctx.render(fitToBounds(elem, Bounds(0, 0, width, height)))

    svg(
      svgAttrs.width := width,
      svgAttrs.height := height,
      svgAttrs.xmlns := "http://www.w3.org/2000/svg"
    )(ctx.elems.toSeq: _*)

  }

  implicit val shapeRenderer = new Renderer[ShapeElem, ScalaTagRC] {
    def render(ctx: ScalaTagRC, elem: ShapeElem): Unit = {
      val tx =
        ctx.getTransform.applyBefore(elem.tx.applyBefore(elem.shape.currentTransform))
      val shape = elem.shape
      if (
        elem.fill.a > 0d || (elem.stroke.isDefined && elem.strokeColor.a > 0)
      ) {
        val svgShape = shape match {
          case Rectangle(x1, y1, w1, h1, _, _) => {
            rect(
              x := x1.toString,
              y := y1,
              svgAttrs.width := w1,
              svgAttrs.height := h1,
              svgAttrs.transform := tx.svg
            )
          }
          case Ellipse(x, y, w, h, _) => {
            val centerX = x + .5 * w
            val centerY = y + .5 * h
            val radiusX = w * .5
            val radiusY = h * .5
            ellipse(
              cx := centerX,
              cy := centerY,
              rx := radiusX,
              ry := radiusY,
              svgAttrs.transform := tx.svg
            )
          }
          case Line(a, b, c, d, _) => {
            svgTags.line(
              x1 := a,
              y1 := b,
              x2 := c,
              y2 := d,
              svgAttrs.transform := tx.svg
            )
          }
          case SimplePath(ps, _) => {
            polyline(
              points := ps
                .map { p =>
                  p.x.toString + " " + p.y
                }
                .mkString(" "),
              svgAttrs.transform := tx.svg
            )
          }
          case Path(ops, _) => {
            path(
              d := ops map {
                case MoveTo(Point(x, y))                  => s"M$x,$y"
                case LineTo(Point(x, y))                  => s"L$x,$y"
                case QuadTo(Point(x2, y2), Point(x1, y1)) => s"Q$x1,$y1,$x2,$y2"
                case CubicTo(Point(x3, y3), Point(x1, y1), Point(x2, y2)) =>
                  s"C$x1,$y1,$x2,$y2,$x3,$y3"
              } mkString (" "),
              svgAttrs.transform := tx.svg
            )
          }
        }

        val filled =
          if (elem.fill.a > 0.0)
            svgShape(fill := elem.fill.css)
          else svgShape(fill := "none")

        val stroked =
          if (elem.stroke.isDefined && elem.strokeColor.a > 0)
            filled(
              stroke := elem.strokeColor.css,
              strokeWidth := elem.stroke.get.width,
              strokeDasharray := elem.stroke.get.dash.mkString(" "),
              strokeLinecap := (elem.stroke.get.cap match {
                case CapRound  => "round"
                case CapButt   => "butt"
                case CapSquare => "square"
              })
            )
          else filled

        ctx.elems.append(stroked)
      }
    }
  }

  implicit val textRenderer = new Renderer[TextBox, ScalaTagRC] {
    def render(ctx: ScalaTagRC, elem: TextBox): Unit = {
      if (!elem.layout.isEmpty) {
        elem.layout.lines.foreach { case (line, lineTx) =>
          val tx = ctx.getTransform.applyBefore(elem.tx.applyBefore(lineTx))
          val svgElem = text(
            svgAttrs.x := 0,
            svgAttrs.y := 0,
            svgAttrs.transform := tx.svg,
            svgAttrs.style := s"font-family: monospace;font-size: ${elem.font.size}"
          )(line)
          ctx.elems.append(svgElem)
        }
      }
    }
  }

}
