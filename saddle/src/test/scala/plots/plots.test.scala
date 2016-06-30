package org.nspl
import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.nspl.saddle._
import org.nspl.data._
import org.saddle._

import org.saddle.io._

class SaddlePlotSpec extends FunSpec with Matchers {

  def readFrameFromClasspath(s: String) =
    CsvParser.parse(new CsvSource {
      val is = scala.io.Source.fromInputStream(
        getClass.getResourceAsStream(s)
      ).getLines
      def readLine = {
        if (is.hasNext) is.next
        else null
      }

    }).withColIndex(0)

  describe("plots") {

    it("plot gallery") {
      val evec = readFrameFromClasspath("/evec.csv").mapValues(CsvParser.parseDouble _)
      val rotated = readFrameFromClasspath("/rotated.csv").mapValues(CsvParser.parseDouble _)
      val data = readFrameFromClasspath("/data.csv").colAt(Array(0, 1, 2, 3)).mapValues(CsvParser.parseDouble _)
      val species = readFrameFromClasspath("/data.csv").colAt(4)
      val spec2Num = species.toVec.toSeq.distinct.sorted.zipWithIndex.toMap
      val spec: Series[Int, Double] = species.mapValues(spec2Num).mapValues(_.toDouble)

      val eval = readFrameFromClasspath("/sqrteigen.csv").mapValues(CsvParser.parseDouble _).mapValues(x => x * x)

      val data2 = data.joinSPreserveColIx(spec, newColIx = "spec")

      val scree = xyplot(
        indexed(eval.firstCol("x").toVec.toSeq.sorted.reverse) -> line()
      )(
          axisMargin = 0,
          xlab = "Order",
          ylab = "Eval",
          main = "Scree"
        )

      val hist1 = xyplot(
        HistogramData(rotated.firstCol("PC1").toVec.toSeq, 10) -> bar()
      )(xlab = "PC1", ylab = "freq.", main = "Loading distribution")

      val density1 = xyplot(
        density(rotated.firstCol("PC1").toVec.toSeq) -> line()
      )(xlab = "PC1", ylab = "dens.", main = "Loading distribution")

      val r1 = rasterplotFromFrame(rotated, yLabFontSize = 0.1 fts)

      val fig0 = xyplot(
        data2.col("Sepal.Length", "Sepal.Width", "spec")
      )(
          extraLegend = spec2Num.toSeq.map(x => x._1 -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), color = DiscreteColors(spec2Num.size)(x._2))),
          xlab = "Sepal.Length",
          ylab = "Sepal.Width",
          main = "Iris data"
        )

      val fig1 = xyplot(
        Frame((rotated.col("PC1", "PC2").toColSeq :+ ("spec" -> spec)): _*),
        density2d(rotated.firstCol("PC1").toVec.toSeq zip rotated.firstCol("PC2").toVec.toSeq)
      )(
          extraLegend = spec2Num.toSeq.map(x => x._1 -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), color = DiscreteColors(spec2Num.size)(x._2))),
          xlab = "PC1",
          ylab = "PC2",
          main = "PC1 vs PC2"
        )

      val fig2 = xyplot(
        Frame((rotated.col("PC2", "PC3").toColSeq :+ ("spec" -> spec)): _*)
      )(
          extraLegend = spec2Num.toSeq.map(x => x._1 -> PointLegend(shape = Shape.rectangle(0, 0, 1, 1), color = DiscreteColors(spec2Num.size)(x._2))),
          xlab = "PC2",
          ylab = "PC3",
          main = "PC2 vs PC3"
        )

      val pc3 = rotated.firstCol("PC3").toVec.toSeq
      val pc2 = rotated.firstCol("PC2").toVec.toSeq

      val fig3 = binnedboxplot(pc3, pc2, xlab = "PC2", ylab = "PC3")

      val fig4 = boxplot(
        data2.firstCol("Sepal.Length").toVec.toSeq -> data2.firstCol("Sepal.Width").toVec.toSeq,
        ylab = "Sepal Length",
        xnames = Seq("Sepal Length", "Sepal Width")
      )

      val contour = contourplot(
        xlim = -2d -> 2d,
        ylim = -2d -> 2d,
        f = (x, y) => (x * x + y * y - 0.5 + x * y),
        n = 50,
        levels = 10
      )

      val density2 = xyplot(
        density(rotated.firstCol("PC1").toVec.toSeq) -> line()
      )(xlab = "PC1", ylab = "dens.", main = "Loading distribution")

      val empty2 =
        figure(
          xyplotarea(
            Nil,
            AxisSettings(LinearAxisFactory),
            AxisSettings(LinearAxisFactory),
            origin = Some(Point(0.0, 0.0)),
            xlim = Some(-1d -> 1d),
            ylim = Some(-1d -> 1d)
          )
        )

      val rs = 1 to 100 map (i => scala.util.Random.nextGaussian)

      val p6 = rasterplot(rasterFromSeq(rs, 10, 10, MinMaxImpl(0.0, 1.0)), xFontSize = 0.5 fts, yFontSize = 0.5 fts, valueText = true, valueColor = Color.white, valueFontSize = 0.6 fts, xnames = Seq(0.0 -> "sdfsf", 1.0 -> "dsfds adfdf adfs f"), ynames = Seq(0.0 -> "dsfsdf", 2.0 -> "dfsdf asdfdf asdfds sdfadsf"))

      val gallery = group(p6, empty2, xyplot(Seq(0d -> 0d, 1d -> 1d, 2d -> 2d))(), r1, hist1, contour, density1, fig0, fig1, fig2, fig3, fig4, TableLayout(3))

      {
        import awtrenderer._

        show(gallery)
        println(pngToFile(gallery))
        println(pdfToFile(gallery))
        println(renderToFile(gallery, 1000, "image/svg"))
      }
      {
        import scalatagrenderer._
        println(svgToFile(gallery))
      }
    }

  }
}
