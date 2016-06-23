package org.nspl

import org.nspl.data._
import scala.collection.JavaConversions._
import org.saddle._

object saddle {

  def rasterplotFromFrame[RX, CX](
    dataFrame: Frame[RX, CX, Double],
    main: String = "",
    xlab: String = "",
    ylab: String = "",
    xLabFontSize: RelFontSize = 1 fts,
    yLabFontSize: RelFontSize = 1 fts,
    mainFontSize: RelFontSize = 1 fts,
    colormap: Colormap = HeatMapColors(0, 1)
  ) =
    rasterplot(
      asRaster(dataFrame.toMat),
      main,
      xlab,
      ylab,
      xLabFontSize,
      yLabFontSize,
      mainFontSize,
      colormap,
      dataFrame.colIx.toSeq.map(_.toString).zipWithIndex.map(x => x._2.toDouble + 0.5 -> x._1),
      dataFrame.rowIx.toSeq.map(_.toString).zipWithIndex.map(x => x._2.toDouble + 0.5 -> x._1)
    )

  def asRaster(mat: Mat[Double]): DataMatrix =
    new DataMatrix(mat.contents, mat.numCols, mat.numRows)

  implicit def dataSourceFromMat(mat: Mat[Double]): DataTable =
    new DataTable(mat.contents, mat.numCols)

  implicit def dataSourceFrom1DVec(vec: Vec[Double]): DataTable =
    new DataTable(vec, 1)

  def dataSourceFromRowMajorVec(vec: Vec[Double], numCols: Int): DataTable =
    new DataTable(vec, numCols)

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
        frame.toRowSeq.map {
          case (rx, series) =>
            VectorRow(series.toVec.toSeq.toVector, rx.toString)
        }.iterator

      def dimension = frame.numCols

      def columnMinMax(i: Int): MinMax = {
        val v = frame.colAt(i).toVec
        MinMaxImpl(v.min.get, v.max.get)
      }

      def quantilesOfColumn(i: Int, qs: Vector[Double]) = {
        val v = frame.colAt(i).toVec
        percentile(v.toSeq, qs).toVector

      }
    }
}
