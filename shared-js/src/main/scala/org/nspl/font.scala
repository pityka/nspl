package org.nspl

import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html
import org.w3c

object svgFont {
  def apply(f: Font) = s"font-family: ${f.name};font-size: ${f.size}"
  
}

/* Code duplication! */
object canvasFont {
  def apply(f: Font) = s"${f.size}px ${f.name}"
  
}

private[nspl] object CanvasGlyphMeasurer extends Font.GlyphMeasurer {
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
  def lineMetrics(f: Font): Font.LineMetrics = {
    withFont(f) {
      val metric = ctx.measureText(abc).asInstanceOf[scalajs.js.Dynamic]
      Font.LineMetrics(
        metric.actualBoundingBoxAscent.asInstanceOf[Double],
        metric.actualBoundingBoxDescent.asInstanceOf[Double],
        0
      )
    }

  }
}
