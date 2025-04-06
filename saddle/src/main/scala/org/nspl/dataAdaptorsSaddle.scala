package org.nspl

import org.nspl.data._
import org.saddle._

object saddle {

  def barplotVertical[F: FC](
      series: Series[String, Double],
      color: Colormap = Color.white,
      yLabFontSize: Option[RelFontSize] = None
  )(parameters: Parameters) =
    xyplot(
      series.toSeq
        .map(_._2)
        .zipWithIndex
        .map(x => x._1 -> x._2.toDouble) -> bar(
        horizontal = true,
        fill = color,
        fillCol = 0
      )
    )(
      parameters.yLabFontSize(
        yLabFontSize.getOrElse(
          math.min(2d, parameters.yHeight.value / series.length) fts
        )
      )
    )

  def barplotHorizontal[F: FC](
      series: Series[String, Double],
      xLabFontSize: Option[RelFontSize] = None,
      color: Colormap = Color.white
  )(parameters: Parameters) =
    xyplot(
      series.toSeq
        .map(_._2)
        .zipWithIndex
        .map(x => x._1 -> x._2.toDouble)
        .map(_.swap) -> bar(horizontal = false, fill = color, fillCol = 1)
    )(
      parameters
        .xLabFontSize(
          xLabFontSize.getOrElse(
            math.min(2d, parameters.xWidth.value / series.length) fts
          )
        )
        .xnames(
          series.toSeq
            .map(_._1.toString)
            .zipWithIndex
            .map(x => x._1 -> x._2.toDouble)
            .map(_.swap)
        )
    )

  def rasterplotFromFrame[RX, CX, F: FC](
      dataFrame: Frame[RX, CX, Double],
      colormap: Colormap = HeatMapColors(0, 1),
      valueText: Boolean = false,
      valueColor: Color = Color.black,
      valueFontSize: RelFontSize = 0.4 fts,
      zlim: Option[(Double, Double)] = None,
      transparentPixels: Option[Double] = None,
      xTickFontSize: Option[RelFontSize] = None,
      yTickFontSize: Option[RelFontSize] = None
  )(parameters: Parameters) =
    rasterplot(
      asRaster(dataFrame.toMat),
      colormap,
      valueText = valueText,
      valueColor = valueColor,
      valueFontSize = valueFontSize,
      zlim = zlim,
      transparentPixels = transparentPixels
    )(
      parameters
        .xTickFontSize(
          xTickFontSize.getOrElse(
            math.min(2d, parameters.xWidth.value / dataFrame.numCols) fts
          )
        )
        .yTickFontSize(
          yTickFontSize.getOrElse(
            math.min(2d, parameters.yHeight.value / dataFrame.numRows) fts
          )
        )
        .xnames(
          dataFrame.colIx.toSeq
            .map(_.toString)
            .zipWithIndex
            .map(x => x._2.toDouble + 0.5 -> x._1)
        )
        .ynames(
          dataFrame.rowIx.toSeq
            .map(_.toString)
            .zipWithIndex
            .map(x => x._2.toDouble + 0.5 -> x._1)
        )
        .yNumTicks(0)
        .xNumTicks(0)
        .xTickLength(0d fts)
        .yTickLength(0d fts)
    )

  def asRaster(mat: Mat[Double]): DataMatrix =
    new DataMatrix(mat.contents, mat.numCols, mat.numRows)

  implicit def dataSourceFromMat(mat: Mat[Double]): DataTable =
    new DataTable(mat.contents, mat.numCols)

  implicit def dataSourceFrom1DVec(vec: Vec[Double]): DataSourceWithQuantiles =
    indexed(vec.toSeq)

  implicit def dataSourceFromSeries[R](
      s: Series[R, Double]
  ): DataSourceWithQuantiles =
    new DataSourceWithQuantiles {
      def iterator =
        s.toSeq.iterator.zipWithIndex
          .map(x => VectorRow(Vector(x._2.toDouble, x._1._2), x._1._1.toString))
      def dimension = 2
      def columnMinMax(i: Int) = i match {
        case 0 => Some(MinMaxImpl(0, s.length - 1d))
        case 1 => Some(MinMaxImpl(s.min.get, s.max.get))
        case _ => None
      }
      def quantilesOfColumn(i: Int, qs: Vector[Double]) = {
        assert(i == 1 || i == 0)
        val v =
          if (i == 1) s.toVec.toSeq.sorted
          else (0 until s.length).map(_.toDouble)
        percentile(v, qs).toVector
      }
    }

  def dataSourceFromRowMajorVec(vec: Vec[Double], numCols: Int): DataTable =
    new DataTable(vec.contents, numCols)

  implicit def dataSourceFromZippedVecs2(
      vec1: (Vec[Double], Vec[Double])
  ): DataSourceWithQuantiles =
    dataSourceFromFrame(Frame(vec1._1, vec1._2))

  implicit def dataSourceFromZippedVecs3(
      vec1: (Vec[Double], Vec[Double], Vec[Double])
  ): DataSourceWithQuantiles =
    dataSourceFromFrame(Frame(vec1._1, vec1._2, vec1._3))

  implicit def dataSourceFromZippedVecs4(
      vec1: (Vec[Double], Vec[Double], Vec[Double], Vec[Double])
  ): DataSourceWithQuantiles =
    dataSourceFromFrame(Frame(vec1._1, vec1._2, vec1._3, vec1._4))

  implicit def dataSourceFromFrame[RX, CX](
      frame: Frame[RX, CX, Double]
  ): DataSourceWithQuantiles =
    new DataSourceWithQuantiles {

      def iterator =
        frame.toRowSeq.map { case (rx, series) =>
          VectorRow(series.toVec.toSeq.toVector, rx.toString)
        }.iterator

      def dimension = frame.numCols

      override def columnNames: Seq[String] = frame.colIx.toSeq.map(_.toString)

      def columnMinMax(i: Int): Option[MinMax] = {
        if (i < frame.numCols) {
          val v = frame.colAt(i).toVec
          for { min <- v.min; max <- v.max } yield MinMaxImpl(min, max)
        } else None

      }

      def quantilesOfColumn(i: Int, qs: Vector[Double]) = {
        val v = frame.colAt(i).toVec
        percentile(v.toSeq, qs).toVector

      }
    }
}
