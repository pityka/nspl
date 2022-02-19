package org.nspl

import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html
import org.w3c

object svgFont {
  def apply(f: Font) = f match {
    case Monospace => s"font-family: monospace;font-size: ${Monospace.size}"
    case NamedFont(name, size) => s"font-family: ${name};font-size: ${size}"
  }
}

/* Code duplication! */
object canvasFont {
  def apply(f: Font) = f match {
    case Monospace             => s"${Monospace.size}px monospace"
    case NamedFont(name, size) => s"${size}px $name"
  }
}

object CanvasGlyphMeasurer extends GlyphMeasurer[Font#F] {
  val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
  val ctx =
    canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  val abc =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPRQRSTUVWXYZ0123456789%,./][()]"

  var currentFont: Font#F = null
  def withFont[T](font: Font#F)(f: => T) = {
    if (currentFont != null && currentFont != font) {
      currentFont = font
      ctx.font = canvasFont(font)
    }
    f
  }

  val fontWidthCache =
    scala.collection.mutable.AnyRefMap[(Char, Font#F), Double]()

  def advance(s: Char, f: Font#F): Double = {
    fontWidthCache.get((s, f)) match {
      case None =>
        val width = withFont(f) {
          val metric = ctx.measureText(s.toString)
          math.abs(
            metric
              .asInstanceOf[scalajs.js.Dynamic]
              .actualBoundingBoxLeft
              .asInstanceOf[Double]
          ) +
            math.abs(
              metric
                .asInstanceOf[scalajs.js.Dynamic]
                .actualBoundingBoxRight
                .asInstanceOf[Double]
            )
        }
        if (fontWidthCache.size < 5000) {
          fontWidthCache.update((s, f), width)
        }
        width
      case Some(w) => w
    }

  }
  def lineMetrics(f: Font#F): LineMetrics = {
    withFont(f) {
      val metric = ctx.measureText(abc).asInstanceOf[scalajs.js.Dynamic]
      LineMetrics(
        metric.actualBoundingBoxAscent.asInstanceOf[Double],
        metric.actualBoundingBoxDescent.asInstanceOf[Double],
        0
      )
    }

  }
}
