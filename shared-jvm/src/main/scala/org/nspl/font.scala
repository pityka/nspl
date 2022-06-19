package org.nspl
import java.awt.{Font => JFont}

private[nspl] object JavaFontConversion {
  def font2font(myFont: Font): JFont =  new JFont(myFont.name, JFont.PLAIN, myFont.size)
  

}

private[nspl] object AwtGlyphMeasurer extends Font.GlyphMeasurer {
  import JavaFontConversion._
  import java.awt.image.BufferedImage
  val bimage = new BufferedImage(50, 50, BufferedImage.TYPE_BYTE_BINARY)
  val g2d = bimage.createGraphics();
  val frc = g2d.getFontRenderContext
  val abc =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPRQRSTUVWXYZ0123456789%,./][()]"
  def advance(s: Char, f: Font): Double =
    font2font(f).getStringBounds(s.toString, frc).getWidth
  def lineMetrics(f: Font): Font.LineMetrics = {
    val lm = font2font(f).getLineMetrics(abc, frc)
    Font.LineMetrics(lm.getAscent, lm.getDescent, lm.getLeading)
  }
}
