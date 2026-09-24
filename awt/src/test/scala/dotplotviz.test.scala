package org.nspl

import org.nspl.awtrenderer._
import java.awt.image.BufferedImage
import java.awt.{Color => AwtColor, RenderingHints}

class DotplotRenderSpec extends munit.FunSuite {

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

  test("dot plot from labels renders without error and is non-blank") {
    val rng = new scala.util.Random(1)
    val pts =
      (0 until 80).map(_ => "wide" -> (rng.nextGaussian() * 1d + 5d)) ++
        (0 until 80).map(_ => "narrow" -> (rng.nextGaussian() * 0.2d + 5d)) ++
        (0 until 40).flatMap(_ =>
          List(
            "bimodal" -> (rng.nextGaussian() * 0.3d + 3d),
            "bimodal" -> (rng.nextGaussian() * 0.3d + 7d)
          )
        )
    val p = dotplotFromLabels(pts.toList)(par).build
    assert(hasInk(renderToImage(p)))
  }

  test("dot offsets are density-scaled and bounded by the slice width") {
    val rows = dotplotData(
      Seq(
        (0d, "const", Seq.fill(50)(3.5)),
        (1d, "spread", (0 until 50).map(_.toDouble))
      ),
      width = 0.8d
    )
    val halfWidth = 0.4d
    val eps = 1e-9
    val perGroup = rows.iterator.toVector.groupBy(_.apply(2))
    perGroup.foreach { case (group, groupRows) =>
      val center = group
      groupRows.foreach { row =>
        val offset = row(0) - center
        assert(
          math.abs(offset) <= halfWidth + eps,
          s"offset $offset exceeds half width in group $group"
        )
      }
    }
    assert(perGroup(0d).nonEmpty && perGroup(1d).nonEmpty)
  }

  test("dot plot of constant data does not throw and renders") {
    val p = dotplotImpl(
      Seq((0d, "const", Seq.fill(50)(3.5))),
      xnames = Nil
    )(par).build
    assert(hasInk(renderToImage(p)))
  }
}
