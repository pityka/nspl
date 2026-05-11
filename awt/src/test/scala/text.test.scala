package org.nspl

import org.nspl.awtrenderer._
import java.awt.image.BufferedImage
import java.awt.{Color => AwtColor, RenderingHints}

class TextBoxBoundsSpec extends munit.FunSuite {

  private val width = 600
  private val height = 200

  private def mkContext(): (BufferedImage, JavaRC) = {
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g2d = img.createGraphics()
    g2d.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON
    )
    g2d.setRenderingHint(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      RenderingHints.VALUE_TEXT_ANTIALIAS_ON
    )
    g2d.setColor(AwtColor.WHITE)
    g2d.fillRect(0, 0, width, height)
    g2d.setColor(AwtColor.BLACK)
    val ctx = new JavaRC(g2d, doRender = true)
    (img, ctx)
  }

  /** Bounding box of non-white pixels in `img`, or `None` if the image is
    * fully white.
    */
  private def inkBounds(img: BufferedImage): Option[Bounds] = {
    var minX = Int.MaxValue
    var minY = Int.MaxValue
    var maxX = Int.MinValue
    var maxY = Int.MinValue
    val w = img.getWidth
    val h = img.getHeight
    var y = 0
    while (y < h) {
      var x = 0
      while (x < w) {
        val argb = img.getRGB(x, y)
        val a = (argb >>> 24) & 0xff
        val r = (argb >>> 16) & 0xff
        val g = (argb >>> 8) & 0xff
        val b = argb & 0xff
        // Treat anything visibly darker than near-white as ink. Stay above
        // the antialias fringe; we want a stable ink-bbox.
        val isInk = a > 16 && (r < 200 || g < 200 || b < 200)
        if (isInk) {
          if (x < minX) minX = x
          if (y < minY) minY = y
          if (x > maxX) maxX = x
          if (y > maxY) maxY = y
        }
        x += 1
      }
      y += 1
    }
    if (minX > maxX) None
    else Some(Bounds(minX.toDouble, minY.toDouble, (maxX - minX).toDouble, (maxY - minY).toDouble))
  }

  /** True if `inner` is fully inside `outer` with a tolerance of `slack`
    * pixels on every side. */
  private def encloses(outer: Bounds, inner: Bounds, slack: Double): Boolean =
    inner.x >= outer.x - slack &&
      inner.y >= outer.y - slack &&
      inner.maxX <= outer.maxX + slack &&
      inner.maxY <= outer.maxY + slack

  test("TextBox.bounds encloses the rendered ink (plain text)") {
    val (img, ctx) = mkContext()
    val tb = TextBox("Hello world", fontSize = 2 fts)
    textRenderer.render(ctx, tb)
    val ink = inkBounds(img).getOrElse(fail("nothing was rendered"))
    val predicted = tb.bounds
    // Most fonts have advance widths > glyph ink widths (italic exceeds on
    // the right by the slant); accept a few pixels of slack on each side.
    assert(
      encloses(predicted, ink, slack = 4),
      s"predicted=$predicted does not enclose ink=$ink"
    )
    // Sanity: ink is non-degenerate.
    assert(ink.w > 10, s"ink too narrow: $ink")
    assert(ink.h > 5, s"ink too short: $ink")
  }

  test("TextBox.bounds has positive width / height for nontrivial text") {
    val tb = TextBox("Hello world", fontSize = 1.5 fts)
    assert(tb.bounds.w > 0, s"width was ${tb.bounds.w}")
    assert(tb.bounds.h > 0, s"height was ${tb.bounds.h}")
  }

  test("empty TextBox has zero bounds and renders nothing") {
    val (img, ctx) = mkContext()
    val tb = TextBox("")
    assertEquals(tb.bounds, Bounds(0, 0, 0, 0))
    textRenderer.render(ctx, tb)
    assertEquals(inkBounds(img), None)
  }

  test("bold ink is at least as wide horizontally as plain ink") {
    val (imgP, ctxP) = mkContext()
    val (imgB, ctxB) = mkContext()
    val plain = TextBox("Hello", fontSize = 3 fts)
    val bold = TextBox("Hello", fontSize = 3 fts, bold = true)
    textRenderer.render(ctxP, plain)
    textRenderer.render(ctxB, bold)
    val ip = inkBounds(imgP).get
    val ib = inkBounds(imgB).get
    // Bold glyphs have thicker strokes — ink mass differs even if the
    // advance widths are the same. We test the simplest invariant: bold
    // ink is not narrower than plain ink minus a small antialiasing
    // wiggle.
    assert(ib.w + 2 >= ip.w, s"bold width ${ib.w} unexpectedly narrower than plain ${ip.w}")
  }

  test("oblique ink extends to the right of plain ink") {
    val (imgP, ctxP) = mkContext()
    val (imgO, ctxO) = mkContext()
    val plain = TextBox("M", fontSize = 4 fts)
    val obl = TextBox("M", fontSize = 4 fts, oblique = true)
    textRenderer.render(ctxP, plain)
    textRenderer.render(ctxO, obl)
    val ip = inkBounds(imgP).get
    val io = inkBounds(imgO).get
    // Italic slant pushes the top-right of the glyph past the plain
    // glyph's right edge. Allow a 1-pixel wiggle.
    assert(io.maxX + 1 >= ip.maxX, s"oblique maxX ${io.maxX} not ≥ plain maxX ${ip.maxX}")
  }

  test("underline adds ink below the glyph baseline that plain text lacks") {
    val (imgP, ctxP) = mkContext()
    val (imgU, ctxU) = mkContext()
    val plain = TextBox("text", fontSize = 4 fts)
    val under = TextBox("text", fontSize = 4 fts, underline = true)
    textRenderer.render(ctxP, plain)
    textRenderer.render(ctxU, under)
    val ip = inkBounds(imgP).get
    val iu = inkBounds(imgU).get
    // Underlined ink reaches further down (descender region) than plain.
    assert(
      iu.maxY > ip.maxY,
      s"underlined maxY ${iu.maxY} should exceed plain maxY ${ip.maxY}"
    )
  }

  test("superscript ink sits above subscript ink for the same string") {
    val (imgSup, ctxSup) = mkContext()
    val (imgSub, ctxSub) = mkContext()
    val sup = TextBox("x", fontSize = 4 fts, superScript = true)
    val sub = TextBox("x", fontSize = 4 fts, subScript = true)
    textRenderer.render(ctxSup, sup)
    textRenderer.render(ctxSub, sub)
    val isup = inkBounds(imgSup).get
    val isub = inkBounds(imgSub).get
    // The centroid (or just the centerY) of the superscript glyph must
    // sit strictly above the centroid of the subscript glyph.
    val supCenter = (isup.y + isup.maxY) * 0.5
    val subCenter = (isub.y + isub.maxY) * 0.5
    assert(
      supCenter < subCenter,
      s"super centerY $supCenter not above sub centerY $subCenter"
    )
  }

  test("multi-line wrapped text bounds enclose the ink") {
    val (img, ctx) = mkContext()
    val tb = TextBox(
      "the quick brown fox jumps over the lazy dog",
      width = Some(60d),
      fontSize = 1.5 fts
    )
    textRenderer.render(ctx, tb)
    val ink = inkBounds(img).getOrElse(fail("nothing rendered"))
    assert(
      encloses(tb.bounds, ink, slack = 4),
      s"wrapped: predicted=${tb.bounds} did not enclose ink=$ink"
    )
    // Wrapped onto more than one line, so height > one-line height.
    val oneLine = TextBox("the", fontSize = 1.5 fts)
    assert(
      tb.bounds.h > oneLine.bounds.h * 1.5,
      s"wrapped height ${tb.bounds.h} should be > 1.5 * single line ${oneLine.bounds.h}"
    )
  }
}
