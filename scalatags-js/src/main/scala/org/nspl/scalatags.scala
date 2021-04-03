package org.nspl

import scalatags.JsDom.all._
import scalatags.JsDom.svgTags._
import scalatags.JsDom.svgAttrs._
import scalatags.JsDom.svgAttrs
import scalatags.JsDom.svgTags
import scalatags.jsdom.Frag
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw._
import org.scalajs.dom.ext._

case class ScalaTagRC(elems: scala.collection.mutable.ArrayBuffer[Frag])
    extends RenderingContext

object scalatagrenderer {

  implicit val defaultGlyphMeasurer = CanvasGlyphMeasurer

  implicit val defaultAWTFont: FontConfiguration = importFont("Arial")

  implicit def rec2bounds(r: ClientRect) =
    Bounds(r.left, r.top, r.width, r.height)

  type SER[T] = Renderer[T, ScalaTagRC]

  def cssColor(c: org.nspl.Color) = f"#${c.r}%02x${c.g}%02x${c.b}%02x"

  implicit class Pimp[K <: Renderable[K]](t: K) {
    def render(ctx: ScalaTagRC)(implicit r: SER[K]) = r.render(ctx, t)
  }

  implicit class PimpedTx(tx: AffineTransform) {
    def svg = {
      s"matrix(${tx.m0}, ${tx.m3}, ${tx.m1}, ${tx.m4}, ${tx.m2}, ${tx.m5})"
    }
  }

  def renderToScalaTag[K <: Renderable[K]](
      elem: Build[K],
      width: Int = 1000
  )(implicit
      er: SER[K]
  ) = {

    val ctx = ScalaTagRC(scala.collection.mutable.ArrayBuffer[Frag]())

    val rootElem = svg(
      svgAttrs.width := width,
      svgAttrs.xmlns := "http://www.w3.org/2000/svg"
    ).render

    var paintableElem = elem.build
    var dragStart = Point(0, 0)
    var mousedown = false

    def paint = {
      val aspect = paintableElem.bounds.h / paintableElem.bounds.w
      val height = (width * aspect)
      rootElem.setAttribute("height", height.toString)

      fitToBounds(paintableElem, Bounds(0, 0, width, height)).render(ctx)
      // rootElem.children.foreach(n => { println(n); rootElem.removeChild(n) })
      while (rootElem.firstChild != null) {
        rootElem.removeChild(rootElem.firstChild)
      }
      ctx.elems.foreach(n => rootElem.appendChild(n.render))
      ctx.elems.clear()
    }

    def getCanvasCoordinate(e: MouseEvent): Point = {
      val rect = rootElem.getBoundingClientRect()
      val x = e.clientX - rect.left;
      val y = e.clientY - rect.top;
      Point(x, y)
    }

    paint

    rootElem

  }

  implicit val shapeRenderer = new SER[ShapeElem] {
    def render(ctx: ScalaTagRC, elem: ShapeElem): Unit = {
      if (
        elem.fill.a > 0d || (elem.stroke.isDefined && elem.strokeColor.a > 0)
      ) {
        val svgShape = elem.shape match {
          case Rectangle(x1, y1, w1, h1, tx, _) => {
            rect(
              x := x1.toString,
              y := y1,
              svgAttrs.width := w1,
              svgAttrs.height := h1,
              svgAttrs.transform := tx.svg
            )
          }
          case Ellipse(x, y, w, h, tx) => {
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
          case Line(a, b, c, d) => {
            svgTags.line(x1 := a, y1 := b, x2 := c, y2 := d)
          }
          case SimplePath(ps) => {
            polyline(
              points := ps
                .map { p =>
                  p.x.toString + " " + p.y.toString
                }
                .mkString(" ")
            )
          }
          case Path(ops) => {
            path(
              d := ops map {
                case MoveTo(Point(x, y))                  => s"M$x,$y"
                case LineTo(Point(x, y))                  => s"L$x,$y"
                case QuadTo(Point(x2, y2), Point(x1, y1)) => s"Q$x1,$y1,$x2,$y2"
                case _                                    => ???
                // case CubicTo(Point(x3, y3), Point(x1, y1), Point(x2, y2)) => path.curveTo(x1, y1, x2, y2, x3, y3)
              } mkString (" ")
            )
          }
        }

        val filled =
          if (elem.fill.a > 0.0)
            svgShape(fill := cssColor(elem.fill))
          else svgShape(fill := "none")

        val stroked =
          if (elem.stroke.isDefined && elem.strokeColor.a > 0)
            filled(
              stroke := cssColor(elem.strokeColor),
              strokeWidth := elem.stroke.get.width,
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

  implicit val textRenderer = new SER[TextBox] {
    def render(ctx: ScalaTagRC, elem: TextBox): Unit = {
      if (!elem.layout.isEmpty) {
        elem.layout.lines.foreach { case (line, lineTx) =>
          val tx = elem.txLoc.concat(lineTx)
          val svgElem = text(
            svgAttrs.x := 0,
            svgAttrs.y := 0,
            svgAttrs.transform := tx.svg,
            svgAttrs.style := svgFont(elem.font)
          )(line)
          ctx.elems.append(svgElem)
        }
      }
    }
  }

}
