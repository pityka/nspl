package org.nspl

import scalatags.Text.all._
import scalatags.Text.svgTags._
import scalatags.Text.svgAttrs._
import scalatags.Text.svgAttrs
import scalatags.Text.svgTags

case class ScalaTagRC(elems: scala.collection.mutable.ArrayBuffer[Modifier]) extends RenderingContext

object scalatagrenderer {

  type SER[T] = Renderer[T, ScalaTagRC]

  implicit class PimpedColor(c: Color) {
    def css = f"#${c.r}%02x${c.g}%02x${c.b}%02x"
  }

  implicit class Pimp[K <: Renderable[K]](t: K) {
    def render(ctx: ScalaTagRC)(implicit r: SER[K]) = r.render(ctx, t)
  }

  implicit class PimpedTx(tx: AffineTransform) {
    def svg = {
      s"matrix(${tx.m0}, ${tx.m3}, ${tx.m1}, ${tx.m4}, ${tx.m2}, ${tx.m5})"
    }
  }

  def svgToFile[K <: Renderable[K]](
    elem: K,
    width: Int = 1000
  )(
    implicit
    er: SER[K]
  ): java.io.File = {
    import java.io._
    val f = File.createTempFile("nspl", ".svg")

    svgToFile(f, elem, width)
  }

  def svgToFile[K <: Renderable[K]](
    f: java.io.File,
    elem: K,
    width: Int
  )(
    implicit
    er: SER[K]
  ): java.io.File = {
    import java.io._
    val os = new FileOutputStream(f)
    val str: String = renderToScalaTag(elem, width).render
    os.write("""<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
""".getBytes)
    os.write(str.getBytes)
    os.close
    f
  }

  def renderToScalaTag[K <: Renderable[K]](
    elem: K,
    width: Int = 1000
  )(
    implicit
    er: SER[K]
  ) = {

    val ctx = ScalaTagRC(scala.collection.mutable.ArrayBuffer[Modifier]())

    val aspect = elem.bounds.h / elem.bounds.w
    val height = (width * aspect).toInt

    fitToBounds(elem, Bounds(0, 0, width, height)).render(ctx)

    svg(svgAttrs.width := width, svgAttrs.height := height,
      svgAttrs.xmlns := "http://www.w3.org/2000/svg")(ctx.elems: _*)

  }

  implicit val shapeRenderer = new SER[ShapeElem] {
    def render(ctx: ScalaTagRC, elem: ShapeElem): Unit = {

      val svgShape = elem.shape match {
        case Rectangle(x1, y1, w1, h1, tx) => {
          rect(x := x1.toString, y := y1, svgAttrs.width := w1, svgAttrs.height := h1, svgAttrs.transform := tx.svg)
        }
        case Ellipse(x, y, w, h, tx) => {
          val centerX = x + .5 * w
          val centerY = y + .5 * h
          val radiusX = w * .5
          val radiusY = h * .5
          ellipse(cx := centerX, cy := centerY, rx := radiusX, ry := radiusY, svgAttrs.transform := tx.svg)
        }
        case Line(a, b, c, d) => {
          svgTags.line(x1 := a, y1 := b, x2 := c, y2 := d)
        }
        case SimplePath(ps) => {
          polyline(
            points := ps.map { p =>
              p.x + " " + p.y
            }.mkString(" ")
          )
        }
        case Path(ops) => {
          path(
            d := ops map {
              case MoveTo(Point(x, y)) => s"M$x,$y"
              case LineTo(Point(x, y)) => s"L$x,$y"
              case QuadTo(Point(x2, y2), Point(x1, y1)) => s"Q$x1,$y1,$x2,$y2"
              // case CubicTo(Point(x3, y3), Point(x1, y1), Point(x2, y2)) => path.curveTo(x1, y1, x2, y2, x3, y3)
            } mkString (" ")
          )
        }
      }

      val filled = if (elem.fill.a > 0.0)
        svgShape(fill := elem.fill.css)
      else svgShape(fill := "none")

      val stroked = if (elem.stroke.isDefined && elem.strokeColor.a > 0)
        filled(
          stroke := elem.strokeColor.css,
          strokeWidth := elem.stroke.get.width,
          strokeLinecap := (elem.stroke.get.cap match {
            case CapRound => "round"
            case CapButt => "butt"
            case CapSquare => "square"
          })
        )
      else filled

      ctx.elems.append(stroked)

    }
  }

  implicit val textRenderer = new SER[TextBox] {

    def render(ctx: ScalaTagRC, elem: TextBox): Unit = {

      if (elem.text.size > 0) {

        val words = elem.text.split(" ");
        var line = "";
        var y1 = elem.loc.y

        words.zipWithIndex.foreach {
          case (word, n) =>
            val testLine = line + word + " ";
            val testWidth: Double = testLine.size * elem.fontSize * 0.6
            if (elem.width.isDefined && testWidth > elem.width.get && n > 0) {

              val svgElem = svgTags.text(
                svgAttrs.x := elem.loc.x,
                svgAttrs.y := y1 + elem.fontSize,
                svgAttrs.transform := elem.tx.svg,
                svgAttrs.style := s"font-family: monospace;font-size: ${elem.fontSize.toInt}"
              )(line)

              ctx.elems.append(svgElem)

              line = word + " "
              y1 += elem.fontSize
            } else {
              line = testLine
            }
        }
        val svgElem = text(
          svgAttrs.x := elem.loc.x,
          svgAttrs.y := y1 + elem.fontSize,
          svgAttrs.transform := elem.tx.svg,
          svgAttrs.style := s"font-family: monospace;font-size: ${elem.fontSize.toInt}"
        )(line)

        ctx.elems.append(svgElem)

      }
    }
  }

}
