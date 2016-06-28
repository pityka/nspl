package org.nspl

import data._

sealed trait LegendConfig
case object NotInLegend extends LegendConfig
case class InLegend(text: String) extends LegendConfig

/* Factory methods for common plots. */
trait SimplePlots {

  type XYPlot = Elems2[Figure[XYPlotArea], Legend]

  def xyplot(data: (DataSource, List[DataRenderer], LegendConfig)*)(
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
    axisMargin: Double = 0.05,
    legendFontSize: RelFontSize = 1 fts,
    legendWidth: RelFontSize = 30 fts,
    xgrid: Boolean = true,
    ygrid: Boolean = true,
    xWidth: RelFontSize = 20 fts,
    yHeight: RelFontSize = 20 fts
  ): XYPlot = {
    val xFac = if (xlog) Log10AxisFactory else LinearAxisFactory
    val yFac = if (ylog) Log10AxisFactory else LinearAxisFactory

    val originX = if (xlog) 1.0 else 0.0
    val originY = if (ylog) 1.0 else 0.0

    val data1 = if (draw1Line)
      (dataSourceFromRows(List(0.0 -> 1.0)), List(polynom())) +: data.map(x => x._1 -> x._2)
    else data.map(x => x._1 -> x._2)

    val legend1 =
      legend(
        entries = (
          (
            (data map {
              case (ds, render, conf) =>
                conf match {
                  case NotInLegend => None
                  case InLegend(name) => Some(name -> render.map(_.asLegend).find(_.isDefined).flatten)
                }
            }).filter(x => x.isDefined && x.get._2.isDefined)
            .map(_.get)
          ).map(x => x._1 -> x._2.get) ++ extraLegend
        ).toList,
        fontSize = legendFontSize,
        width = legendWidth
      )

    group(
      figure(
        xyplotarea(
          data1,
          AxisSettings(
            xFac,
            customTicks = xnames,
            fontSize = xLabFontSize,
            numTicks = xNumTicks,
            width = xWidth
          ),
          AxisSettings(
            yFac,
            customTicks = ynames,
            fontSize = yLabFontSize,
            numTicks = yNumTicks,
            tickLabelDistance = 0.0 fts,
            width = yHeight
          ),
          None,
          xlim = xlim,
          ylim = ylim,
          axisMargin = axisMargin,
          xgrid = xgrid,
          ygrid = ygrid
        ),
        main = main,
        xlab = xlab,
        ylab = ylab,
        xlabFontSize = xLabFontSize,
        ylabFontSize = yLabFontSize,
        mainFontSize = mainFontSize
      ),
      legend1,
      HorizontalStack(Center)
    )
  }

  type BoxPlot = Figure[XYPlotArea]

  def boxplot(
    data: DataSourceWithQuantiles,
    main: String = "",
    xlab: String = "",
    ylab: String = "",
    xnames: Seq[String] = Nil,
    fontSize: RelFontSize = 1 fts,
    xgrid: Boolean = true,
    ygrid: Boolean = true,
    xWidth: RelFontSize = 20 fts,
    yHeight: RelFontSize = 20 fts
  ): BoxPlot = {

    val bxdata = boxplotData(data)

    val min = bxdata.iterator.map(_(4)).min
    val max = bxdata.iterator.map(_(5)).max

    figure(
      xyplotarea(
        List(bxdata -> List(boxwhisker())),
        AxisSettings(
          LinearAxisFactory,
          customTicks = xnames.zipWithIndex.map(x => x._2.toDouble + 1 -> x._1),
          numTicks = 0,
          fontSize = fontSize,
          width = xWidth
        ),
        AxisSettings(
          LinearAxisFactory,
          fontSize = fontSize,
          tickLabelDistance = 0.0 fts,
          width = yHeight
        ),
        None,
        xlim = Some(0d -> (bxdata.iterator.size + 1)),
        ylim = Some(min -> max),
        xgrid = xgrid,
        ygrid = ygrid
      ),
      main = main,
      xlab = xlab,
      ylab = ylab,
      xlabFontSize = fontSize,
      ylabFontSize = fontSize,
      mainFontSize = fontSize
    )
  }

  def binnedboxplot(
    dim1: Seq[Double],
    dim2: Seq[Double],
    main: String = "",
    xlab: String = "",
    ylab: String = "",
    xnames: Seq[String] = Nil,
    fontSize: RelFontSize = 1 fts,
    bins: Int = 10,
    xgrid: Boolean = true,
    ygrid: Boolean = true,
    xWidth: RelFontSize = 20 fts,
    yHeight: RelFontSize = 20 fts
  ): XYPlot = {

    xyplot(
      boxplotData(
        dim1,
        dim2,
        1 to bins map (i => i / bins.toDouble),
        Vector.fill(bins)(0d)
      ) -> boxwhisker()
    )(
        xlab = xlab,
        ylab = ylab,
        xlim = Some(dim2.min -> dim2.max),
        ylim = Some(dim1.min -> dim1.max),
        xLabFontSize = fontSize,
        yLabFontSize = fontSize,
        mainFontSize = fontSize,
        xgrid = xgrid,
        ygrid = ygrid,
        xWidth = xWidth,
        yHeight = yHeight
      )
  }

  def contourplot(
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
    yHeight: RelFontSize = 20 fts
  ): XYPlot = {

    val contours = data.contour(
      xlim._1,
      xlim._2, ylim._1, ylim._2, n, levels
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
        yHeight = yHeight
      )
  }

  type RasterPlot = Elems2[Figure[XYPlotArea], HeatmapLegend]

  def rasterplot(
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
    yHeight: RelFontSize = 20 fts
  ): RasterPlot = {
    val minmaxx = data.columnMinMax(xCol)
    val minmaxy = data.columnMinMax(yCol)
    val minmaxz = data.columnMinMax(zCol)
    val xmin = minmaxx.min
    val xmax = minmaxx.max
    val ymin = minmaxy.min
    val ymax = minmaxy.max
    val zmin = minmaxz.min
    val zmax = minmaxz.max

    group(
      figure(
        xyplotarea(
          List(data -> List(point(
            pointSizeIsInDataSpaceUnits = true,
            color = colormap.withRange(zmin, zmax),
            shapes = Vector(Shape.rectangle(0.0, 0.0, 1.0, 1.0)),
            size = 1d
          ))),
          AxisSettings(
            LinearAxisFactory,
            customTicks = xnames,
            fontSize = xFontSize,
            numTicks = (if (xnames.isEmpty) (xmax - xmin).toInt else 0),
            numMinorTicksFactor = 0,
            tickLength = 0 fts,
            labelRotation = -.5 * math.Pi,
            width = xWidth
          ),
          AxisSettings(
            LinearAxisFactory,
            customTicks = ynames,
            fontSize = yFontSize,
            numTicks = (if (ynames.isEmpty) (ymax - ymin).toInt else 0),
            numMinorTicksFactor = 0,
            tickLabelDistance = 0.0 fts,
            tickLength = 0 fts,
            width = yHeight
          ),
          None,
          xlim = Some(xmin -> (xmax + 1)),
          ylim = Some(ymin -> (ymax + 1)),
          axisMargin = 0.0
        ),
        main = main,
        xlab = xlab,
        ylab = ylab,
        xlabFontSize = xFontSize,
        ylabFontSize = yFontSize,
        mainFontSize = mainFontSize
      ),
      heatmapLegend(zmin, zmax, colormap),
      HorizontalStack(Center)
    )

  }
}
