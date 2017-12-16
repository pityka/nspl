package org.nspl
import java.awt.{Font => JFont}

object JavaFontConversion {
  implicit def font2font(myFont: Font): JFont = myFont match {
    case Monospace             => new JFont(JFont.MONOSPACED, JFont.PLAIN, Monospace.size)
    case NamedFont(name, size) => new JFont(name, JFont.PLAIN, size)
  }

}

object AwtGlyphMeasurer extends GlyphMeasurer[Font#F] {
  import JavaFontConversion._
  import java.awt.image.BufferedImage
  val bimage = new BufferedImage(50, 50, BufferedImage.TYPE_BYTE_BINARY)
  val g2d = bimage.createGraphics();
  val frc = g2d.getFontRenderContext
  val abc =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPRQRSTUVWXYZ0123456789%,./][()]"
  def advance(s: Char, f: Font#F): Double =
    font2font(f).getStringBounds(s.toString, frc).getWidth
  def lineMetrics(f: Font#F): LineMetrics = {
    val lm = font2font(f).getLineMetrics(abc, frc)
    LineMetrics(lm.getAscent, lm.getDescent, lm.getLeading)
  }
}
