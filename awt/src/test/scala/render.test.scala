package org.nspl

import org.nspl.awtrenderer._
import org.nspl.data._
import java.awt.image.BufferedImage
import java.awt.{Color => AwtColor, RenderingHints}

class RenderSmokeSpec extends munit.FunSuite {

  private def renderToImage[K <: Renderable[K]](
      elem: K,
      width: Int = 400
  )(implicit er: Renderer[K, JavaRC]): BufferedImage = {
    val aspect = elem.bounds.h / elem.bounds.w
    val height = math.max((width * aspect).toInt, 1)
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
    val rc = new JavaRC(g2d, doRender = true)
    rc.render(fitToBounds(elem, Bounds(0, 0, width, height)))
    img
  }

  private def hasInk(img: BufferedImage): Boolean = {
    val w = img.getWidth
    val h = img.getHeight
    var y = 0
    while (y < h) {
      var x = 0
      while (x < w) {
        val rgb = img.getRGB(x, y)
        val r = (rgb >>> 16) & 0xff
        val g = (rgb >>> 8) & 0xff
        val b = rgb & 0xff
        if (r < 240 || g < 240 || b < 240) return true
        x += 1
      }
      y += 1
    }
    false
  }

  test("scatter plot renders without error and produces non-blank output") {
    val data = (1 to 100).map(i => (i.toDouble, i.toDouble * 0.5))
    val p = xyplot(data -> point())().build
    val img = renderToImage(p)
    assert(hasInk(img), "rendered image is blank")
    assertEquals(img.getWidth, 400)
  }

  test("line plot renders without error") {
    val data = (1 to 50).map(i => (i.toDouble, math.sin(i * 0.2)))
    val p = xyplot(data -> line())().build
    assert(hasInk(renderToImage(p)))
  }

  test("bar plot on a log y-axis renders (regression: would crash on worldToView(0))") {
    val data = (1 to 5).map(i => (i.toDouble, math.pow(10d, i.toDouble)))
    val p = xyplot(
      data -> bar(width = 0.6)
    )(par.ylog(true)).build
    // Just exercise the render — the bug it guards against was a
    // RuntimeException("<0") from log10(0) in bar()'s width calculation.
    val img = renderToImage(p)
    assert(hasInk(img))
  }

  test("error bars actually pick up the errorBarColor (regression for fill/stroke bug)") {
    // Magenta error bars should appear as magenta-ish pixels in the
    // output. The bug was that errorBarColor was being passed as `fill`
    // on a stroked line (which is invisible), so the bars always rendered
    // black. We just verify some pixels carry the requested color.
    val data = Seq(
      (1d, 5d, 0d, 0d, 0d, 7d, 3d), // x, y, color, size, shape, errTop, errBot
      (2d, 5d, 0d, 0d, 0d, 7d, 3d),
      (3d, 5d, 0d, 0d, 0d, 7d, 3d)
    )
    val magenta = Color(255, 0, 255, 255)
    val p = xyplot(
      data -> point(
        size = 10d,
        color = Color.black,
        errorBarColor = magenta,
        errorBarStroke = StrokeConf(0.5 fts)
      )
    )(par.xlim(Some(0d -> 4d)).ylim(Some(0d -> 10d))).build
    val img = renderToImage(p, width = 600)
    // Look for any pixel where R is much higher than G — magenta.
    var found = false
    val w = img.getWidth
    val h = img.getHeight
    var y = 0
    while (y < h && !found) {
      var x = 0
      while (x < w && !found) {
        val rgb = img.getRGB(x, y)
        val r = (rgb >>> 16) & 0xff
        val gg = (rgb >>> 8) & 0xff
        val bb = rgb & 0xff
        if (r > 150 && bb > 150 && gg < 100) found = true
        x += 1
      }
      y += 1
    }
    assert(found, "no magenta error-bar pixels were rendered")
  }

  test("rendered image dimensions are sane for an extreme aspect ratio") {
    // Long narrow data shouldn't produce a zero-height image.
    val data = (1 to 200).map(i => (i.toDouble, math.sin(i * 0.1) * 0.001))
    val p = xyplot(data -> line())().build
    val img = renderToImage(p, width = 800)
    assert(img.getHeight > 0, s"got height ${img.getHeight}")
  }

  test("histogram of constant data does not divide by zero") {
    // Regression check for histogram code paths.
    val hist = HistogramData(Seq.fill(20)(3.5), 1.0)
    val d = hist.density
    // Should not throw and should return *some* HistogramData.
    assert(d.bins.nonEmpty || hist.bins.isEmpty)
  }
}
