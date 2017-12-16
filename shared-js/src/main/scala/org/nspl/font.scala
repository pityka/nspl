package org.nspl

import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.dom.html

object svgFont {
  def apply(f: Font) = f match {
    case Monospace             => s"font-family: monospace;font-size: ${Monospace.size}"
    case NamedFont(name, size) => s"font-family: ${name};font-size: ${size}"
  }
}

/* Code duplication! */
object canvasFont {
  def apply(f: Font) = f match {
    case Monospace             => s"${Monospace.size}pt monospace"
    case NamedFont(name, size) => s"${size}pt $name"
  }
}

object CanvasGlyphMeasurer extends GlyphMeasurer[Font#F] {
  val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
  val ctx =
    canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  val abc =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPRQRSTUVWXYZ0123456789%,./][()]"
  def advance(s: Char, f: Font#F): Double = {
    ctx.font = canvasFont(f)
    ctx.measureText(s.toString).width
  }
  def lineMetrics(f: Font#F): LineMetrics = {
    ctx.font = canvasFont(f)
    LineMetrics(f.size.toDouble * 0.78, descent = f.size.toDouble * 0.22, 0)
  }
}
