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

object CanvasGlyphMeasurer extends GlyphMeasurer[Font] {
  val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
  val ctx =
    canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  val abc =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPRQRSTUVWXYZ0123456789%,./][()]"

  var currentFont: Font = null
  def withFont[T](font: Font)(f: => T) = {
    if (currentFont != null && currentFont != font) {
      currentFont = font
      ctx.font = canvasFont(font)
    }
    f
  }

  val fontWidthCache =
    scala.collection.mutable.Map[Char, Double]()
  var fontOfCache: Font = null

  def advance(s: Char, f: Font): Double = {
    if (fontOfCache != null && fontOfCache != f)
      ctx.measureText(s.toString).width
    else {
      fontOfCache = f
      fontWidthCache.get(s) match {
        case None =>
          val width = withFont(f) {
            ctx.measureText(s.toString).width
          }
          if (fontWidthCache.size < 5000) {
            fontWidthCache.update(s, width)
          }
          width
        case Some(w) => w
      }
    }

  }
  def lineMetrics(f: Font): LineMetrics = {
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
