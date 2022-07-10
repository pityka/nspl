package org.nspl

/** Holds settings and parameters applicable for a different plots.
    *
    * See [[org.nspl.par]]
    */
  class Parameters(
      val xlog: Boolean,
      val ylog: Boolean,
      val main: String,
      val xlab: String,
      val ylab: String,
      val xnames: Seq[(Double, String)],
      val ynames: Seq[(Double, String)],
      val xlim: Option[(Double, Double)],
      val ylim: Option[(Double, Double)],
      val draw1Line: Boolean,
      val extraLegend: Seq[(String, LegendElem)],
      val xLabFontSize: RelFontSize,
      val yLabFontSize: RelFontSize,
      val mainFontSize: RelFontSize,
      val xNumTicks: Int,
      val yNumTicks: Int,
      val xAxisMargin: Double,
      val yAxisMargin: Double,
      val legendFontSize: RelFontSize,
      val legendWidth: RelFontSize,
      val xgrid: Boolean,
      val ygrid: Boolean,
      val xWidth: RelFontSize,
      val yHeight: RelFontSize,
      val frame: Boolean,
      val xLabelRotation: Double,
      val yLabelRotation: Double,
      val origin: Option[(Double, Double)],
      val xCustomGrid: Boolean,
      val yCustomGrid: Boolean,
      val legendLayout: Layout,
      val legendDistance: RelFontSize,
      val xTickLength: RelFontSize,
      val yTickLength: RelFontSize,
      val xLineWidthFraction: Double,
      val yLineWidthFraction: Double,
      val xLineStartFraction: Double,
      val yLineStartFraction: Double,
      val topPadding: RelFontSize,
      val bottomPadding: RelFontSize,
      val leftPadding: RelFontSize,
      val rightPadding: RelFontSize,
      val xLabDistance: RelFontSize,
      val yLabDistance: RelFontSize,
      val mainLabDistance: RelFontSize,
      val xTickSpace: Option[Double],
      val yTickSpace: Option[Double],
      val noLegend: Boolean,
      val xNoTickLabel: Boolean,
      val yNoTickLabel: Boolean
  ) {
    def copy(
        yNoTickLabel: Boolean = yNoTickLabel,
        xNoTickLabel: Boolean = xNoTickLabel,
        noLegend: Boolean = noLegend,
        yTickSpace: Option[Double] = yTickSpace,
        xTickSpace: Option[Double] = xTickSpace,
        mainLabDistance: RelFontSize = mainLabDistance,
        yLabDistance: RelFontSize = yLabDistance,
        xLabDistance: RelFontSize = xLabDistance,
        rightPadding: RelFontSize = rightPadding,
        leftPadding: RelFontSize = leftPadding,
        bottomPadding: RelFontSize = bottomPadding,
        topPadding: RelFontSize = topPadding,
        yLineStartFraction: Double = yLineStartFraction,
        xLineStartFraction: Double = xLineStartFraction,
        yLineWidthFraction: Double = yLineWidthFraction,
        xLineWidthFraction: Double = xLineWidthFraction,
        yTickLength: RelFontSize = yTickLength,
        xTickLength: RelFontSize = xTickLength,
        legendDistance: RelFontSize = legendDistance,
        legendLayout: Layout = legendLayout,
        yCustomGrid: Boolean = yCustomGrid,
        xCustomGrid: Boolean = xCustomGrid,
        origin: Option[(Double, Double)] = origin,
        yLabelRotation: Double = yLabelRotation,
        xLabelRotation: Double = xLabelRotation,
        frame: Boolean = frame,
        yHeight: RelFontSize = yHeight,
        xWidth: RelFontSize = xWidth,
        ygrid: Boolean = ygrid,
        xgrid: Boolean = xgrid,
        legendWidth: RelFontSize = legendWidth,
        legendFontSize: RelFontSize = legendFontSize,
        yAxisMargin: Double = yAxisMargin,
        xAxisMargin: Double = xAxisMargin,
        yNumTicks: Int = yNumTicks,
        xNumTicks: Int = xNumTicks,
        mainFontSize: RelFontSize = mainFontSize,
        yLabFontSize: RelFontSize = yLabFontSize,
        xLabFontSize: RelFontSize = xLabFontSize,
        extraLegend: Seq[(String, LegendElem)] = extraLegend,
        draw1Line: Boolean = draw1Line,
        ylim: Option[(Double, Double)] = ylim,
        xlim: Option[(Double, Double)] = xlim,
        ynames: Seq[(Double, String)] = ynames,
        xnames: Seq[(Double, String)] = xnames,
        ylab: String = ylab,
        xlab: String = xlab,
        main: String = main,
        ylog: Boolean = ylog,
        xlog: Boolean = xlog
    ) = new Parameters(
      xlog = xlog,
      ylog = ylog,
      main = main,
      xlab = xlab,
      ylab = ylab,
      xnames = xnames,
      ynames = ynames,
      xlim = xlim,
      ylim = ylim,
      draw1Line = draw1Line,
      extraLegend = extraLegend,
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
      origin = origin,
      xCustomGrid = xCustomGrid,
      yCustomGrid = yCustomGrid,
      legendLayout = legendLayout,
      legendDistance = legendDistance,
      xTickLength = xTickLength,
      yTickLength = yTickLength,
      xLineWidthFraction = xLineWidthFraction,
      yLineWidthFraction = yLineWidthFraction,
      xLineStartFraction = xLineStartFraction,
      yLineStartFraction = yLineStartFraction,
      topPadding = topPadding,
      bottomPadding = bottomPadding,
      leftPadding = leftPadding,
      rightPadding = rightPadding,
      xLabDistance = xLabDistance,
      yLabDistance = yLabDistance,
      mainLabDistance = mainLabDistance,
      xTickSpace = xTickSpace,
      yTickSpace = yTickSpace,
      noLegend = noLegend,
      xNoTickLabel = xNoTickLabel,
      yNoTickLabel = yNoTickLabel
    )
  }

  /** Factory method shorthand for Parameters */
  object par {

    /** Factory method of [[org.nspl.Parameters]]
      * @param xlog
      *   x axis is in log space
      * @param ylog
      *   y axis is in log space
      * @param main
      *   main plot label (title)
      * @param xlab
      *   x axis label
      * @param ylab
      *   y axis label
      * @param xnames
      *   custom ticks on the x axis with location (in world/axis space) and
      *   label
      * @param ynames
      *   custo ticks on the y axis
      * @param xlim
      *   limits (range) of the x axis, data driven if empty
      * @param ylim
      *   limits (range) of the y axis, data driven if empty
      * @param draw1Line
      *   draws the y=x line
      * @param extraLegend
      *   Items to be added in the plot legend in addition of those derived from
      *   the data sources
      * @param xLabFontSize
      *   font size of xlab
      * @param yLabFontSize
      *   font size of ylab
      * @param mainFontSize
      *   font size of main
      * @param xNumTicks
      *   number of ticks on the x axis
      * @param yNumTicks
      *   number of ticks on the y axis
      * @param xAxisMargin
      *   Defines the limit of the x axis. If xlim is empty then the x axis
      *   range is (xmax-xmin) * (1+xAxisMargin)
      * @param yAxisMargin
      *   Defines the limit of the y axis.
      * @param legendFontSize
      *   Font size of the legend
      * @param legendWidth
      *   Width of the legend
      * @param xgrid
      *   Draw background grid at major ticks on x axis
      * @param ygrid
      *   Draw background grid at major ticks on y axis
      * @param xWidth
      *   Width of the x axis in relative font size units
      * @param yHeight
      *   height of the y axis in relative font size units
      * @param frame
      *   Draw a rectangular frame around the plot area
      * @param xLabelRotation
      *   x axis label rotation
      * @param yLabelRotation
      *   y axis label rotation
      * @param origin
      *   World/Axis coordinate of where the two axes will intersect. Data
      *   driven if empty.
      * @param xCustomGrid
      *   custom grid lines on x axis
      * @param yCustomGrid
      *   custom grid line son y axis
      * @param legendLayout
      *   Layout of the legend items
      * @param legendDistance
      *   Distance of the legend items from other legend items
      * @param xTickLength
      *   tick length on x axis
      * @param yTickLength
      *   tick length on y axis
      * @param xLineWidthFraction
      *   The length of the line visually representing the x axis as a fraction
      *   of the logical range of the axis
      * @param yLineWidthFraction
      *   The length of the line visually representing the y axis as a fraction
      *   of the logical range of the axis
      * @param xLineStartFraction
      *   The start of the line which represents the x axis
      * @param yLineStartFraction
      *   The start of the line which represents the x axis
      * @param topPadding
      *   padding around the whole plot (includes xlab, ylab, main labels)
      * @param bottomPadding
      *   padding around the whole plot
      * @param leftPadding
      *   padding around the whole plot
      * @param rightPadding
      *   padding around the whole plot
      * @param xLabDistance
      *   xlab to axis distance
      * @param yLabDistance
      *   ylab to axis distance
      * @param mainLabDistance
      *   main to frame distance
      * @param xTickSpace
      *   x axis tick spacing, automatic if empty, recommended empty
      * @param yTickSpace
      *   y axis tick spacing, automatic if empty, recommended empty
      * @param noLegend
      *   skip drawing legend
      * @param xNoTickLabel
      *   skip drawing tick labels on x axis
      * @param yNoTickLabel
      *   skip drawing tick labels on y axis
      */
    def apply(
        noLegend: Boolean = false,
        yTickSpace: Option[Double] = None,
        xTickSpace: Option[Double] = None,
        mainLabDistance: RelFontSize = 0.75 fts,
        yLabDistance: RelFontSize = 0.5 fts,
        xLabDistance: RelFontSize = 0.5 fts,
        rightPadding: RelFontSize = 5d fts,
        leftPadding: RelFontSize = 5d fts,
        bottomPadding: RelFontSize = 3d fts,
        topPadding: RelFontSize = 3d fts,
        yLineStartFraction: Double = 0d,
        xLineStartFraction: Double = 0d,
        yLineWidthFraction: Double = 1d,
        xLineWidthFraction: Double = 1d,
        yTickLength: RelFontSize = 0.4 fts,
        xTickLength: RelFontSize = 0.4 fts,
        legendDistance: RelFontSize = 0.5 fts,
        legendLayout: Layout = ColumnLayout(
          numRows = 10,
          horizontalGap = 0.75 fts,
          verticalGap = 0.4 fts
        ),
        yCustomGrid: Boolean = false,
        xCustomGrid: Boolean = false,
        origin: Option[(Double, Double)] = None,
        yLabelRotation: Double = 0d,
        xLabelRotation: Double = 0d,
        frame: Boolean = true,
        yHeight: RelFontSize = 20 fts,
        xWidth: RelFontSize = 20 fts,
        ygrid: Boolean = true,
        xgrid: Boolean = true,
        legendWidth: RelFontSize = 30 fts,
        legendFontSize: RelFontSize = 1 fts,
        yAxisMargin: Double = 0.05,
        xAxisMargin: Double = 0.05,
        yNumTicks: Int = 6,
        xNumTicks: Int = 6,
        mainFontSize: RelFontSize = 1 fts,
        yLabFontSize: RelFontSize = 1 fts,
        xLabFontSize: RelFontSize = 1 fts,
        extraLegend: Seq[(String, LegendElem)] = Nil,
        draw1Line: Boolean = false,
        ylim: Option[(Double, Double)] = None,
        xlim: Option[(Double, Double)] = None,
        ynames: Seq[(Double, String)] = Nil,
        xnames: Seq[(Double, String)] = Nil,
        ylab: String = "",
        xlab: String = "",
        main: String = "",
        ylog: Boolean = false,
        xlog: Boolean = false,
        xNoTickLabel: Boolean = false,
        yNoTickLabel: Boolean = false
    ) = new Parameters(
      xlog = xlog,
      ylog = ylog,
      main = main,
      xlab = xlab,
      ylab = ylab,
      xnames = xnames,
      ynames = ynames,
      xlim = xlim,
      ylim = ylim,
      draw1Line = draw1Line,
      extraLegend = extraLegend,
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
      origin = origin,
      xCustomGrid = xCustomGrid,
      yCustomGrid = yCustomGrid,
      legendLayout = legendLayout,
      legendDistance = legendDistance,
      xTickLength = xTickLength,
      yTickLength = yTickLength,
      xLineWidthFraction = xLineWidthFraction,
      yLineWidthFraction = yLineWidthFraction,
      xLineStartFraction = xLineStartFraction,
      yLineStartFraction = yLineStartFraction,
      topPadding = topPadding,
      bottomPadding = bottomPadding,
      leftPadding = leftPadding,
      rightPadding = rightPadding,
      xLabDistance = xLabDistance,
      yLabDistance = yLabDistance,
      mainLabDistance = mainLabDistance,
      xTickSpace = xTickSpace,
      yTickSpace = yTickSpace,
      noLegend = noLegend,
      xNoTickLabel = xNoTickLabel,
      yNoTickLabel = yNoTickLabel
    )
  }
