package org.nspl

import data._

sealed trait LegendConfig
case object NotInLegend extends LegendConfig
case class InLegend(text: String) extends LegendConfig

trait CommonOptions {
  def main: String
  def xlab: String
  def ylab: String
  def xnames: Seq[(Double, String)]
  def ynames: Seq[(Double, String)]
  def xlim: Option[(Double, Double)]
  def ylim: Option[(Double, Double)]
  def xLabFontSize: RelFontSize
  def yLabFontSize: RelFontSize
  def mainFontSize: RelFontSize
  def xNumTicks: Int
  def yNumTicks: Int
  def axisMargin: Double
  def legendFontSize: RelFontSize
  def legendWidth: RelFontSize
  def xgrid: Boolean
  def ygrid: Boolean
  def xWidth: RelFontSize
  def yHeight: RelFontSize
  def frame: Boolean
}

/* Factory methods for common plots. */
trait SimplePlots {

  // def options(
  //   main: String = "",
  //   xlab: String = "",
  //   ylab: String = "",
  //   xnames: Seq[(Double, String)] = Seq(),
  //   ynames: Seq[(Double, String)] = Seq(),
  //   xlim: Option[(Double, Double)] = None,
  //   ylim: Option[(Double, Double)] = None,
  //   xLabFontSize: RelFontSize = 1 fts,
  //   yLabFontSize: RelFontSize = 1 fts,
  //   mainFontSize: RelFontSize = 1 fts,
  //   xNumTicks: Int = 4,
  //   yNumTicks: Int = 4,
  //   axisMargin: Double = 0.05,
  //   legendFontSize: RelFontSize = 1 fts,
  //   legendWidth: RelFontSize = 30 fts,
  //   xgrid: Boolean = true,
  //   ygrid: Boolean = true,
  //   xWidth: RelFontSize = 20 fts,
  //   yHeight: RelFontSize = 20 fts,
  //   frame: Boolean = true
  // ) = new CommonOptions {
  //   def main = _main
  //   def xlab = _xlab
  //   def ylab = _ylab
  //   def xnames = _xnames
  //   def ynames = _ynames
  //   def xlim = _xlim
  //   def ylim = _ylim
  //   def xLabFontSize = _xLabFontSize
  //   def yLabFontSize: RelFontSize = 1 fts,
  //   def mainFontSize: RelFontSize = 1 fts,
  //   def xNumTicks: Int = 4,
  //   def yNumTicks: Int = 4,
  //   def axisMargin: Double = 0.05,
  //   def legendFontSize: RelFontSize = 1 fts,
  //   def legendWidth: RelFontSize = 30 fts,
  //   def xgrid: Boolean = true,
  //   def ygrid: Boolean = true,
  //   def xWidth: RelFontSize = 20 fts,
  //   def yHeight: RelFontSize = 20 fts,
  //   def frame: Boolean = true
  // }

  def xyplot[F: FC](data: (DataSource, List[DataRenderer], LegendConfig)*)(
      xlog: Boolean = false,
      ylog: Boolean = false,
      main: String = "",
      xlab: String = "",
      ylab: String = "",
      xnames: Seq[(Double, String)] = Seq(),
      ynames: Seq[(Double, String)] = Seq(),
      xlim: Option[(Double, Double)] = None,
      ylim: Option[(Double, Double)] = None,
      draw1Line: Boolean = false,
      extraLegend: Seq[(String, LegendElem)] = Nil,
      xLabFontSize: RelFontSize = 1 fts,
      yLabFontSize: RelFontSize = 1 fts,
      mainFontSize: RelFontSize = 1 fts,
      xNumTicks: Int = 4,
      yNumTicks: Int = 4,
      xAxisMargin: Double = 0.05,
      yAxisMargin: Double = 0.05,
      legendFontSize: RelFontSize = 1 fts,
      legendWidth: RelFontSize = 30 fts,
      xgrid: Boolean = true,
      ygrid: Boolean = true,
      xWidth: RelFontSize = 20 fts,
      yHeight: RelFontSize = 20 fts,
      frame: Boolean = true,
      xLabelRotation: Double = 0d,
      yLabelRotation: Double = 0d,
      origin: Option[(Double, Double)] = None,
      xCustomGrid: Boolean = false,
      yCustomGrid: Boolean = false,
      legendLayout: Layout = ColumnLayout(
        numRows = 10,
        horizontalGap = 0.75 fts,
        verticalGap = 0.4 fts
      ),
      legendDistance: RelFontSize = 0.5 fts,
      xTickLength: RelFontSize = 0.4 fts,
      yTickLength: RelFontSize = 0.4 fts,
      xLineWidthFraction: Double = 1d,
      yLineWidthFraction: Double = 1d,
      xLineStartFraction: Double = 0d,
      yLineStartFraction: Double = 0d,
      topPadding: RelFontSize = 0d fts,
      bottomPadding: RelFontSize = 0d fts,
      leftPadding: RelFontSize = 0d fts,
      rightPadding: RelFontSize = 0d fts,
      xLabDistance: RelFontSize = 0.5 fts,
      yLabDistance: RelFontSize = 0.5 fts,
      mainLabDistance: RelFontSize = 0.75 fts
  ) = {
    val xFac = if (xlog) Log10AxisFactory else LinearAxisFactory
    val yFac = if (ylog) Log10AxisFactory else LinearAxisFactory

    val originX = if (xlog) 1.0 else 0.0
    val originY = if (ylog) 1.0 else 0.0

    val data1 =
      if (draw1Line)
        (
          dataSourceFromRows(List(0.0 -> 1.0)),
          List(polynom(() => line()))
        ) +: data
          .map(x => x._1 -> x._2)
      else data.map(x => x._1 -> x._2)

    val legend1 =
      legend(
        entries = (
          (
            (data map { case (ds, render, conf) =>
              conf match {
                case NotInLegend => None
                case InLegend(name) =>
                  Some(
                    name -> render.map(_.asLegend).find(_.isDefined).flatten
                  )
              }
            }).filter(x => x.isDefined && x.get._2.isDefined).map(_.get)
          ).map(x => x._1 -> x._2.get) ++ extraLegend
        ).toList.distinct,
        fontSize = legendFontSize,
        width = legendWidth,
        legendLayout
      )

    val plotArea =
      xyplotareaBuild(
        data1,
        AxisSettings(
          xFac,
          customTicks = xnames,
          fontSize = xLabFontSize,
          numTicks = xNumTicks,
          width = xWidth,
          labelRotation = xLabelRotation,
          tickLength = xTickLength,
          lineLengthFraction = xLineWidthFraction,
          lineStartFraction = xLineStartFraction
        ),
        AxisSettings(
          yFac,
          customTicks = ynames,
          fontSize = yLabFontSize,
          numTicks = yNumTicks,
          width = yHeight,
          labelRotation = yLabelRotation,
          tickLength = yTickLength,
          lineLengthFraction = yLineWidthFraction,
          lineStartFraction = yLineStartFraction
        ),
        origin = origin.map(x => Point(x._1, x._2)),
        xlim = xlim,
        ylim = ylim,
        xAxisMargin = xAxisMargin,
        yAxisMargin = yAxisMargin,
        xgrid = xgrid,
        ygrid = ygrid,
        frame = frame,
        xCustomGrid = xCustomGrid,
        yCustomGrid = yCustomGrid,
        main = main,
        xlab = xlab,
        ylab = ylab,
        xlabFontSize = xLabFontSize,
        ylabFontSize = yLabFontSize,
        mainFontSize = mainFontSize,
        topPadding = topPadding,
        bottomPadding = bottomPadding,
        leftPadding = leftPadding,
        rightPadding = rightPadding,
        xlabDistance = xLabDistance,
        ylabDistance = yLabDistance,
        mainDistance = mainLabDistance
      )

    group(
      plotArea,
      legend1,
      HorizontalStack(Anchor, legendDistance)
    )
  }
  def xyzplot[F: FC](data: (DataSource, List[DataRenderer3D], LegendConfig)*)(
      zNear: Double = 1d,
      zFar: Double = 2000d,
      fieldOfViewAngles: Double = 60,
      cameraPosition: Math3D.Vec3 = Math3D.Vec3(50f, 50f, 300f),
      cameraTarget: Math3D.Vec3 = Math3D.Vec3(0f, 0f, 0f),
      xWidth: RelFontSize = 20 fts,
      yHeight: RelFontSize = 20 fts,
      // main: String = "",
      // mainFontSize: RelFontSize = 1 fts,
      extraLegend: Seq[(String, LegendElem)] = Nil,
      legendFontSize: RelFontSize = 1 fts,
      legendWidth: RelFontSize = 30 fts,
      legendLayout: Layout = ColumnLayout(
        numRows = 10,
        horizontalGap = 0.75 fts,
        verticalGap = 0.4 fts
      ),
      legendDistance: RelFontSize = 0.5 fts,
      topPadding: RelFontSize = 0d fts,
      bottomPadding: RelFontSize = 0d fts,
      leftPadding: RelFontSize = 0d fts,
      rightPadding: RelFontSize = 0d fts
      // mainLabDistance: RelFontSize = 0.75 fts
  ) = {

    val data1 =
      data.map(x => x._1 -> x._2)

    val legend1 =
      legend(
        entries = (
          (
            (data map { case (ds, render, conf) =>
              conf match {
                case NotInLegend => None
                case InLegend(name) =>
                  Some(
                    name -> render.map(_.asLegend).find(_.isDefined).flatten
                  )
              }
            }).filter(x => x.isDefined && x.get._2.isDefined).map(_.get)
          ).map(x => x._1 -> x._2.get) ++ extraLegend
        ).toList.distinct,
        fontSize = legendFontSize,
        width = legendWidth,
        legendLayout
      )

    val aspect = xWidth.value / yHeight.value

    val plotArea =
      xyzplotareaBuild(
        data1,
        aspect,
        zNear,
        zFar,
        fieldOfViewAngles,
        cameraPosition,
        cameraTarget,
        topPadding,
        bottomPadding,
        leftPadding,
        rightPadding,
        xWidth,
        yHeight
      )

    group(
      plotArea,
      legend1,
      HorizontalStack(Anchor, legendDistance)
    )
  }

  def stackedBarPlot[F: FC](
      data: DataSource,
      legend: Seq[(Int, String, Colormap)],
      xCol: Int = 0,
      relative: Boolean = false,
      main: String = "",
      xlab: String = "",
      ylab: String = "",
      xnames: Seq[(Double, String)] = Seq(),
      ynames: Seq[(Double, String)] = Seq(),
      xlim: Option[(Double, Double)] = None,
      ylim: Option[(Double, Double)] = None,
      xLabFontSize: RelFontSize = 1 fts,
      yLabFontSize: RelFontSize = 1 fts,
      mainFontSize: RelFontSize = 1 fts,
      xNumTicks: Int = 4,
      yNumTicks: Int = 4,
      xAxisMargin: Double = 0.05,
      yAxisMargin: Double = 0.05,
      legendFontSize: RelFontSize = 1 fts,
      legendWidth: RelFontSize = 30 fts,
      xgrid: Boolean = true,
      ygrid: Boolean = true,
      xWidth: RelFontSize = 20 fts,
      yHeight: RelFontSize = 20 fts,
      frame: Boolean = true,
      xLabelRotation: Double = 0d,
      yLabelRotation: Double = 0d,
      xLabDistance: RelFontSize = 0.5 fts,
      yLabDistance: RelFontSize = 0.5 fts,
      mainLabDistance: RelFontSize = 0.75 fts
  ) = {
    {
      val data1: Seq[Seq[VectorRow]] = data.iterator
        .map { row =>
          val x = row(xCol)
          val sum = if (relative) legend.map(x => row(x._1)).sum else 1.0

          val data = legend.map(x => row(x._1) / sum)

          val accum: Seq[Double] =
            data.drop(1).scanLeft(data.head)((y, l) => y + l)

          accum zip (0.0 +: accum.dropRight(1)) map (y =>
            VectorRow(Vector(x, y._1, y._2), "")
          )

        }
        .toVector
        .transpose

      val legend1 = legend.zipWithIndex.map(x => (x._2, x._1._2, x._1._3))

      val renderers =
        legend1.zip(data1).map { case ((idx, label, color), data) =>
          val ds: DataSource = data
          (
            ds,
            List(
              bar(
                fill = color,
                fillCol = ds.dimension + 1,
                widthCol = ds.dimension + 2,
                yCol = 1,
                yCol2 = Some(2)
              )
            ),
            InLegend(label)
          )
        }

      xyplot(renderers: _*)(
        main = main,
        xlab = xlab,
        ylab = ylab,
        xnames = xnames,
        ynames = ynames,
        xlim = xlim,
        ylim = ylim,
        xLabFontSize = xLabFontSize,
        yLabFontSize = yLabFontSize,
        mainFontSize = mainFontSize,
        xNumTicks = xNumTicks,
        yNumTicks = yNumTicks,
        xAxisMargin = xAxisMargin,
        yAxisMargin = yAxisMargin,
        legendFontSize = legendFontSize,
        legendWidth = legendWidth,
        xgrid = xgrid,
        ygrid = ygrid,
        xWidth = xWidth,
        yHeight = yHeight,
        frame = frame,
        xLabelRotation = xLabelRotation,
        yLabelRotation = yLabelRotation,
        xLabDistance = xLabDistance,
        yLabDistance = yLabDistance,
        mainLabDistance = mainLabDistance
      )

    }
  }

  def boxplot[F: FC](
      data: DataSourceWithQuantiles,
      main: String = "",
      xlab: String = "",
      ylab: String = "",
      xnames: Seq[String] = Nil,
      fontSize: RelFontSize = 1 fts,
      xgrid: Boolean = true,
      ygrid: Boolean = true,
      xWidth: RelFontSize = 20 fts,
      yHeight: RelFontSize = 20 fts,
      boxColor: Colormap = Color.gray4,
      frame: Boolean = true,
      xLabelRotation: Double = 0d,
      yLabelRotation: Double = 0d
  ) = {

    val bxdata = boxplotData(data)

    boxplotImpl(
      bxdata,
      main,
      xlab,
      ylab,
      xnames,
      fontSize,
      xgrid,
      ygrid,
      xWidth,
      yHeight,
      boxColor,
      frame,
      xLabelRotation,
      yLabelRotation
    )
  }

  def boxplotImpl[F: FC](
      bxdata: DataSource,
      main: String = "",
      xlab: String = "",
      ylab: String = "",
      xnames: Seq[String] = Nil,
      fontSize: RelFontSize = 1 fts,
      xgrid: Boolean = true,
      ygrid: Boolean = true,
      xWidth: RelFontSize = 20 fts,
      yHeight: RelFontSize = 20 fts,
      boxColor: Colormap = Color.gray4,
      frame: Boolean = true,
      xLabelRotation: Double = 0d,
      yLabelRotation: Double = 0d
  ) = {

    val min = bxdata.iterator.flatMap(_.allColumns.iterator.drop(1).take(5)).min
    val max = bxdata.iterator.flatMap(_.allColumns.iterator.drop(1).take(5)).max

    xyplotareaBuild(
      List(bxdata -> List(boxwhisker(fill = boxColor))),
      AxisSettings(
        LinearAxisFactory,
        customTicks = xnames.zipWithIndex.map(x => x._2.toDouble + 1 -> x._1),
        numTicks = if (xnames.isEmpty) 5 else 0,
        fontSize = fontSize,
        width = xWidth,
        labelRotation = xLabelRotation
      ),
      AxisSettings(
        LinearAxisFactory,
        fontSize = fontSize,
        width = yHeight,
        labelRotation = yLabelRotation
      ),
      None,
      xlim = Some(0d -> (bxdata.iterator.size + 1)),
      ylim = Some(min -> max),
      xgrid = xgrid,
      ygrid = ygrid,
      frame = frame,
      main = main,
      xlab = xlab,
      ylab = ylab,
      xlabFontSize = fontSize,
      ylabFontSize = fontSize,
      mainFontSize = fontSize
    )
  }

  def boxplotFromLabels[T: Ordering, F: FC](
      data: Seq[(T, Double)],
      main: String = "",
      xlab: String = "",
      ylab: String = "",
      fontSize: RelFontSize = 1 fts,
      xgrid: Boolean = true,
      ygrid: Boolean = true,
      xWidth: RelFontSize = 20 fts,
      yHeight: RelFontSize = 20 fts,
      boxColor: Colormap = Color.gray4,
      frame: Boolean = true,
      xLabelRotation: Double = 0d,
      yLabelRotation: Double = 0d,
      useLabels: Boolean = true
  ) = {
    val bxdata = boxplotData(data.toSeq)

    boxplotImpl(
      bxdata,
      main,
      xlab,
      ylab,
      if (useLabels) bxdata.iterator.map(_.label).toList else Nil,
      fontSize,
      xgrid,
      ygrid,
      xWidth,
      yHeight,
      boxColor,
      frame,
      xLabelRotation,
      yLabelRotation
    )
  }

  def binnedboxplot[F: FC](
      dim1: Seq[Double],
      dim2: Seq[Double],
      main: String = "",
      xlab: String = "",
      ylab: String = "",
      xnames: Seq[String] = Nil,
      fontSize: RelFontSize = 1 fts,
      bins: Int = 10,
      xgrid: Boolean = false,
      ygrid: Boolean = true,
      xWidth: RelFontSize = 20 fts,
      yHeight: RelFontSize = 20 fts,
      boxColor: Colormap = Color.gray4,
      frame: Boolean = true,
      xLabelRotation: Double = 0d,
      yLabelRotation: Double = 0d
  ) = {

    xyplot(
      boxplotData(
        dim1,
        dim2,
        1 to bins map (i => i / bins.toDouble),
        Vector.fill(bins)(0d)
      ) -> boxwhisker(fill = boxColor)
    )(
      xlab = xlab,
      ylab = ylab,
      main = main,
      xlim = Some(dim2.min -> dim2.max),
      ylim = Some(dim1.min -> dim1.max),
      xLabFontSize = fontSize,
      yLabFontSize = fontSize,
      mainFontSize = fontSize,
      xgrid = xgrid,
      ygrid = ygrid,
      xWidth = xWidth,
      yHeight = yHeight,
      frame = frame,
      xLabelRotation = xLabelRotation,
      yLabelRotation = yLabelRotation
    )
  }

  def contourplot[F: FC](
      xlim: (Double, Double),
      ylim: (Double, Double),
      f: (Double, Double) => Double,
      n: Int,
      levels: Int,
      main: String = "",
      xlab: String = "",
      ylab: String = "",
      fontSize: RelFontSize = 1 fts,
      xgrid: Boolean = true,
      ygrid: Boolean = true,
      xWidth: RelFontSize = 20 fts,
      yHeight: RelFontSize = 20 fts,
      frame: Boolean = true
  ) = {
    val contours = data.contour(
      xlim._1,
      xlim._2,
      ylim._1,
      ylim._2,
      n,
      levels
    )(f)

    xyplot(
      linesegments(contours)
    )(
      ylim = Some(ylim),
      xlim = Some(xlim),
      xlab = xlab,
      ylab = ylab,
      main = main,
      xLabFontSize = fontSize,
      yLabFontSize = fontSize,
      mainFontSize = fontSize,
      xgrid = xgrid,
      ygrid = ygrid,
      xWidth = xWidth,
      yHeight = yHeight,
      frame = frame
    )
  }

  def rasterplot[F: FC](
      data: DataSource,
      main: String = "",
      xlab: String = "",
      ylab: String = "",
      xFontSize: RelFontSize = 1 fts,
      yFontSize: RelFontSize = 1 fts,
      mainFontSize: RelFontSize = 1 fts,
      colormap: Colormap = HeatMapColors(0, 1),
      xnames: Seq[(Double, String)] = Seq(),
      ynames: Seq[(Double, String)] = Seq(),
      xCol: Int = 0,
      yCol: Int = 1,
      zCol: Int = 2,
      xWidth: RelFontSize = 20 fts,
      yHeight: RelFontSize = 20 fts,
      valueText: Boolean = false,
      valueColor: Color = Color.black,
      valueFontSize: RelFontSize = 0.4 fts,
      tickLength: RelFontSize = 0.4 fts,
      zlim: Option[(Double, Double)] = None,
      frame: Boolean = true,
      xLabelRotation: Double = -.5 * math.Pi,
      yLabelRotation: Double = 0d,
      xNumTicks: Int = 4,
      yNumTicks: Int = 4,
      xTickSpace: Option[Double] = None,
      yTickSpace: Option[Double] = None,
      transparentPixels: Option[Double] = None
  ) = {
    val minmaxx = data.columnMinMax(xCol)
    val minmaxy = data.columnMinMax(yCol)
    val minmaxz = data.columnMinMax(zCol)
    val xmin = minmaxx.min
    val xmax = minmaxx.max
    val ymin = minmaxy.min
    val ymax = minmaxy.max
    val zmin = zlim.map(_._1).getOrElse(minmaxz.min)
    val zmax = {
      val v = zlim.map(_._2).getOrElse(minmaxz.max)
      if (v == zmin) zmin + 1
      else v
    }

    group(
      xyplotareaBuild(
        List(
          data -> List(
            point(
              pointSizeIsInDataSpaceUnits = true,
              color = colormap.withRange(zmin, zmax),
              shapes = Vector(Shape.rectangle(0.0, -1.0, 1.0, 1.0)),
              size = 1d,
              valueText = valueText,
              labelColor = valueColor,
              labelFontSize = valueFontSize,
              transparent = transparentPixels
            )
          )
        ),
        AxisSettings(
          LinearAxisFactory,
          customTicks = xnames,
          fontSize = xFontSize,
          numTicks = xNumTicks,
          tickSpace = xTickSpace,
          numMinorTicksFactor = 0,
          tickLength = tickLength,
          labelRotation = xLabelRotation,
          width = xWidth
        ),
        AxisSettings(
          LinearAxisFactory,
          customTicks = ynames,
          fontSize = yFontSize,
          numTicks = yNumTicks,
          tickSpace = yTickSpace,
          numMinorTicksFactor = 0,
          tickLength = tickLength,
          labelRotation = yLabelRotation,
          width = yHeight
        ),
        xlim = Some(xmin -> (xmax + 1)),
        ylim = Some(ymin -> (ymax + 1)),
        xAxisMargin = 0.0,
        yAxisMargin = 0d,
        frame = frame,
        main = main,
        xlab = xlab,
        ylab = ylab,
        xlabFontSize = xFontSize,
        ylabFontSize = yFontSize,
        mainFontSize = mainFontSize
      ),
      heatmapLegend(zmin, zmax, colormap),
      HorizontalStack(Center, 1d fts)
    )

  }
}
