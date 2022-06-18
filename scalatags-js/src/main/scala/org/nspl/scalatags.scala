package org.nspl

import scalatags.JsDom.all._
import scalatags.JsDom.svgTags._
import scalatags.JsDom.svgAttrs._
import scalatags.JsDom.svgAttrs
import scalatags.JsDom.svgTags
import scalatags.jsdom.Frag
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.DOMRect
import org.scalajs.dom.html
import org.scalajs.dom.raw._
import org.scalajs.dom.ext._
import scalatags.JsDom

case class ScalaTagRC private[nspl] (
    elems: scala.collection.mutable.ArrayBuffer[scalatags.generic.TypedTag[org.scalajs.dom.Element, org.scalajs.dom.Element,org.scalajs.dom.Node]]
) extends RenderingContext[ScalaTagRC] {

  private[nspl] var transform: AffineTransform = AffineTransform.identity

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

  implicit val defaultGlyphMeasurer: CanvasGlyphMeasurer.type  = CanvasGlyphMeasurer

  implicit val defaultAWTFont: FontConfiguration = org.nspl.font("Arial")

  private[nspl] def rec2bounds(r: DOMRect) =
    Bounds(r.left, r.top, r.width, r.height)

  private[nspl] def cssColor(c: org.nspl.Color) = f"#${c.r}%02x${c.g}%02x${c.b}%02x"

  private[nspl] implicit class PimpedTx(tx: AffineTransform) {
    def svg = {
      s"matrix(${tx.m0}, ${tx.m3}, ${tx.m1}, ${tx.m4}, ${tx.m2}, ${tx.m5})"
    }
  }

  def renderToScalaTag[K <: Renderable[K]](
      elem: Build[K],
      width: Int = 1000
  )(implicit
      er: Renderer[K, ScalaTagRC]
  ) = {

    val ctx = ScalaTagRC(scala.collection.mutable.ArrayBuffer.empty)

    val rootElem = svg(
      svgAttrs.width := width,
      svgAttrs.xmlns := "http://www.w3.org/2000/svg"
    ).render

    var paintableElem = elem.build

    def paint = {
      val aspect = paintableElem.bounds.h / paintableElem.bounds.w
      val height = (width * aspect)
      rootElem.setAttribute("height", height.toString)

      ctx.render(fitToBounds(paintableElem, Bounds(0, 0, width, height)))
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

  implicit val shapeRenderer : Renderer[ShapeElem, ScalaTagRC]= new Renderer[ShapeElem, ScalaTagRC] {
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
                  p.x.toString + " " + p.y.toString
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
            svgShape(fill := cssColor(elem.fill))
          else svgShape(fill := "none")

        val stroked =
          if (elem.stroke.isDefined && elem.strokeColor.a > 0)
            filled(
              stroke := cssColor(elem.strokeColor),
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

  implicit val textRenderer : Renderer[TextBox, ScalaTagRC]= new Renderer[TextBox, ScalaTagRC] {
    def render(ctx: ScalaTagRC, elem: TextBox): Unit = {
      if (!elem.layout.isEmpty) {
        elem.layout.lines.foreach { case (line, lineTx) =>
          val tx = ctx.getTransform.applyBefore(elem.tx.applyBefore(lineTx))
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
