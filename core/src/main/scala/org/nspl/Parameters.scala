package org.nspl

/** Class which holds common settings of plots.
  *
  * This class is immmutable.
  *
  * The intended use of this class is to call the specialized copy methods on
  * the default instance in `org.nspl.par` e.g.
  *
  * {{{org.nspl.par.withXLog(true).withMain("some text")}}} or equivalently
  *
  * {{{org.nspl.par.xlog(true).main("some text")}}}
  *
  * For each member of this class there are two copy methods:
  *
  *   - One following the naming convention `withX..` e.g. `def
  *     withXLog(v:Boolean) : Parameters`
  *   - the other omitting the `with` prefix, e.g. `def xlog(v:Boolean) :
  *     Parameters`
  */
case class Parameters private (
    xlog: Boolean,
    ylog: Boolean,
    main: String,
    xlab: String,
    ylab: String,
    xnames: Seq[(Double, String)],
    ynames: Seq[(Double, String)],
    xlim: Option[(Double, Double)],
    ylim: Option[(Double, Double)],
    draw1Line: Boolean,
    extraLegend: Seq[(String, LegendElem)],
    xLabFontSize: RelFontSize,
    yLabFontSize: RelFontSize,
    mainFontSize: RelFontSize,
    xNumTicks: Int,
    yNumTicks: Int,
    xAxisMargin: Double,
    yAxisMargin: Double,
    legendFontSize: RelFontSize,
    legendWidth: RelFontSize,
    xgrid: Boolean,
    ygrid: Boolean,
    xWidth: RelFontSize,
    yHeight: RelFontSize,
    frame: Boolean,
    xLabelRotation: Double,
    yLabelRotation: Double,
    origin: Option[(Double, Double)],
    xCustomGrid: Boolean,
    yCustomGrid: Boolean,
    legendLayout: Layout,
    legendDistance: RelFontSize,
    xTickLength: RelFontSize,
    yTickLength: RelFontSize,
    xLineWidthFraction: Double,
    yLineWidthFraction: Double,
    xLineStartFraction: Double,
    yLineStartFraction: Double,
    topPadding: RelFontSize,
    bottomPadding: RelFontSize,
    leftPadding: RelFontSize,
    rightPadding: RelFontSize,
    xLabDistance: RelFontSize,
    yLabDistance: RelFontSize,
    mainLabDistance: RelFontSize,
    xTickSpace: Option[Double],
    yTickSpace: Option[Double],
    noLegend: Boolean,
    xNoTickLabel: Boolean,
    yNoTickLabel: Boolean
) {

  /** @param xlog
    *   x axis is in log space
    */
  def xlog(v: Boolean): Parameters = copy(xlog = v)

  /** @param ylog
    *   y axis is in log space
    */
  def ylog(v: Boolean): Parameters = copy(ylog = v)

  /** @param main
    *   main plot label (title)
    */
  def main(v: String): Parameters = copy(main = v)

  /** @param xlab
    *   x axis label
    */
  def xlab(v: String): Parameters = copy(xlab = v)

  /** @param ylab
    *   y axis label
    */
  def ylab(v: String): Parameters = copy(ylab = v)

  /** @param xnames
    *   custom ticks on the x axis with location (in world/axis space) and label
    */
  def xnames(v: Seq[(Double, String)]): Parameters = copy(xnames = v)

  /** @param ynames
    *   custo ticks on the y axis
    */
  def ynames(v: Seq[(Double, String)]): Parameters = copy(ynames = v)

  /** @param xlim
    *   limits (range) of the x axis, data driven if empty
    */
  def xlim(v: Option[(Double, Double)]): Parameters = copy(xlim = v)

  /** @param ylim
    *   limits (range) of the y axis, data driven if empty
    */
  def ylim(v: Option[(Double, Double)]): Parameters = copy(ylim = v)

  /** @param draw1Line
    *   draws the y=x line
    */
  def draw1Line(v: Boolean): Parameters = copy(draw1Line = v)

  /** @param extraLegend
    *   Items to be added in the plot legend in addition of those derived from
    *   the data sources
    */
  def extraLegend(v: Seq[(String, LegendElem)]): Parameters =
    copy(extraLegend = v)

  /** @param xLabFontSize
    *   font size of xlab
    */
  def xLabFontSize(v: RelFontSize): Parameters = copy(xLabFontSize = v)

  /** @param yLabFontSize
    *   font size of ylab
    */
  def yLabFontSize(v: RelFontSize): Parameters = copy(yLabFontSize = v)

  /** @param mainFontSize
    *   font size of main
    */
  def mainFontSize(v: RelFontSize): Parameters = copy(mainFontSize = v)

  /** @param xNumTicks
    *   number of ticks on the x axis
    */
  def xNumTicks(v: Int): Parameters = copy(xNumTicks = v)

  /** @param yNumTicks
    *   number of ticks on the y axis
    */
  def yNumTicks(v: Int): Parameters = copy(yNumTicks = v)

  /** @param xAxisMargin
    *   Defines the limit of the x axis. If xlim is empty then the x axis range
    *   is (xmax-xmin) * (1+xAxisMargin)
    */
  def xAxisMargin(v: Double): Parameters = copy(xAxisMargin = v)

  /** @param yAxisMargin
    *   Defines the limit of the y axis.
    */
  def yAxisMargin(v: Double): Parameters = copy(yAxisMargin = v)

  /** @param legendFontSize
    *   Font size of the legend
    */
  def legendFontSize(v: RelFontSize): Parameters = copy(legendFontSize = v)

  /** @param legendWidth
    *   Width of the legend
    */
  def legendWidth(v: RelFontSize): Parameters = copy(legendWidth = v)

  /** @param xgrid
    *   Draw background grid at major ticks on x axis
    */
  def xgrid(v: Boolean): Parameters = copy(xgrid = v)

  /** @param ygrid
    *   Draw background grid at major ticks on y axis
    */
  def ygrid(v: Boolean): Parameters = copy(ygrid = v)

  /** @param xWidth
    *   Width of the x axis in relative font size units
    */
  def xWidth(v: RelFontSize): Parameters = copy(xWidth = v)

  /** @param yHeight
    *   height of the y axis in relative font size units
    */
  def yHeight(v: RelFontSize): Parameters = copy(yHeight = v)

  /** @param frame
    *   Draw a rectangular frame around the plot area
    */
  def frame(v: Boolean): Parameters = copy(frame = v)

  /** @param xLabelRotation
    *   x axis label rotation
    */
  def xLabelRotation(v: Double): Parameters = copy(xLabelRotation = v)

  /** @param yLabelRotation
    *   y axis label rotation
    */
  def yLabelRotation(v: Double): Parameters = copy(yLabelRotation = v)

  /** @param origin
    *   World/Axis coordinate of where the two axes will intersect. Data driven
    *   if empty.
    */
  def origin(v: Option[(Double, Double)]): Parameters = copy(origin = v)

  /** @param xCustomGrid
    *   custom grid lines on x axis
    */
  def xCustomGrid(v: Boolean): Parameters = copy(xCustomGrid = v)

  /** @param yCustomGrid
    *   custom grid line son y axis
    */
  def yCustomGrid(v: Boolean): Parameters = copy(yCustomGrid = v)

  /** @param legendLayout
    *   Layout of the legend items
    */
  def legendLayout(v: Layout): Parameters = copy(legendLayout = v)

  /** @param legendDistance
    *   Distance of the legend items from other legend items
    */
  def legendDistance(v: RelFontSize): Parameters = copy(legendDistance = v)

  /** @param xTickLength
    *   tick length on x axis
    */
  def xTickLength(v: RelFontSize): Parameters = copy(xTickLength = v)

  /** @param yTickLength
    *   tick length on y axis
    */
  def yTickLength(v: RelFontSize): Parameters = copy(yTickLength = v)

  /** @param xLineWidthFraction
    *   The length of the line visually representing the x axis as a fraction of
    *   the logical range of the axis
    */
  def xLineWidthFraction(v: Double): Parameters = copy(xLineWidthFraction = v)

  /** @param yLineWidthFraction
    *   The length of the line visually representing the y axis as a fraction of
    *   the logical range of the axis
    */
  def yLineWidthFraction(v: Double): Parameters = copy(yLineWidthFraction = v)

  /** @param xLineStartFraction
    *   The start of the line which represents the x axis
    */
  def xLineStartFraction(v: Double): Parameters = copy(xLineStartFraction = v)

  /** @param yLineStartFraction
    *   The start of the line which represents the x axis
    */
  def yLineStartFraction(v: Double): Parameters = copy(yLineStartFraction = v)

  /** @param topPadding
    *   padding around the whole plot (includes xlab, ylab, main labels)
    */
  def topPadding(v: RelFontSize): Parameters = copy(topPadding = v)

  /** @param bottomPadding
    *   padding around the whole plot
    */
  def bottomPadding(v: RelFontSize): Parameters = copy(bottomPadding = v)

  /** @param leftPadding
    *   padding around the whole plot
    */
  def leftPadding(v: RelFontSize): Parameters = copy(leftPadding = v)

  /** @param rightPadding
    *   padding around the whole plot
    */
  def rightPadding(v: RelFontSize): Parameters = copy(rightPadding = v)

  /** @param xLabDistance
    *   xlab to axis distance
    */
  def xLabDistance(v: RelFontSize): Parameters = copy(xLabDistance = v)

  /** @param yLabDistance
    *   ylab to axis distance
    */
  def yLabDistance(v: RelFontSize): Parameters = copy(yLabDistance = v)

  /** @param mainLabDistance
    *   main to frame distance
    */
  def mainLabDistance(v: RelFontSize): Parameters = copy(mainLabDistance = v)

  /** @param xTickSpace
    *   x axis tick spacing, automatic if empty, recommended empty
    */
  def xTickSpace(v: Option[Double]): Parameters = copy(xTickSpace = v)

  /** @param yTickSpace
    *   y axis tick spacing, automatic if empty, recommended empty
    */
  def yTickSpace(v: Option[Double]): Parameters = copy(yTickSpace = v)

  /** @param noLegend
    *   skip drawing legend
    */
  def noLegend(v: Boolean): Parameters = copy(noLegend = v)

  /** @param xNoTickLabel
    *   skip drawing tick labels on x axis
    */
  def xNoTickLabel(v: Boolean): Parameters = copy(xNoTickLabel = v)

  /** @param yNoTickLabel
    *   skip drawing tick labels on y axi
    */
  def yNoTickLabel(v: Boolean): Parameters = copy(yNoTickLabel = v)

  /*@param xlog  x axis is in log space */
  def withXLog(v: Boolean) = copy(xlog = v)

  /*@param ylog  y axis is in log space */
  def withYLog(v: Boolean) = copy(ylog = v)

  /*@param main  main plot label (title) */
  def withMain(v: String) = copy(main = v)

  /*@param xlab  x axis label */
  def withXLab(v: String) = copy(xlab = v)

  /*@param ylab  y axis label */
  def withYLab(v: String) = copy(ylab = v)

  /*@param xnames
   *   custom ticks on the x axis with location (in world/axis space) and label
   */
  def withXNames(v: Seq[(Double, String)]) = copy(xnames = v)

  /*@param ynames  custo ticks on the y axis */
  def withYNames(v: Seq[(Double, String)]) = copy(ynames = v)

  /*@param xlim  limits (range) of the x axis, data driven if empty */
  def withXLim(v: Option[(Double, Double)]) = copy(xlim = v)

  /*@param ylim  limits (range) of the y axis, data driven if empty */
  def withYLim(v: Option[(Double, Double)]) = copy(ylim = v)

  /*@param draw1Line  draws the y=x line */
  def withDraw1Line(v: Boolean) = copy(draw1Line = v)

  /*@param extraLegend
   *   Items to be added in the plot legend in addition of those derived from
   *   the data sources
   */
  def withExtraLegend(v: Seq[(String, LegendElem)]) = copy(extraLegend = v)

  /*@param xLabFontSize  font size of xlab */
  def withXLabFontSize(v: RelFontSize) = copy(xLabFontSize = v)

  /*@param yLabFontSize  font size of ylab */
  def withYLabFontSize(v: RelFontSize) = copy(yLabFontSize = v)

  /*@param mainFontSize  font size of main */
  def withMainFontSize(v: RelFontSize) = copy(mainFontSize = v)

  /*@param xNumTicks  number of ticks on the x axis */
  def withXNumTicks(v: Int) = copy(xNumTicks = v)

  /*@param yNumTicks  number of ticks on the y axis */
  def withYNumTicks(v: Int) = copy(yNumTicks = v)

  /*@param xAxisMargin
   *   Defines the limit of the x axis. If xlim is empty then the x axis range
   *   is (xmax-xmin) * (1+xAxisMargin)
   */
  def withXAxisMargin(v: Double) = copy(xAxisMargin = v)

  /*@param yAxisMargin  Defines the limit of the y axis. */
  def withYAxisMargin(v: Double) = copy(yAxisMargin = v)

  /*@param legendFontSize  Font size of the legend */
  def withLegendFontSize(v: RelFontSize) = copy(legendFontSize = v)

  /*@param legendWidth  Width of the legend */
  def withLegendWidth(v: RelFontSize) = copy(legendWidth = v)

  /*@param xgrid  Draw background grid at major ticks on x axis */
  def withXGrid(v: Boolean) = copy(xgrid = v)

  /*@param ygrid  Draw background grid at major ticks on y axis */
  def withYGrid(v: Boolean) = copy(ygrid = v)

  /*@param xWidth  Width of the x axis in relative font size units */
  def withXWidth(v: RelFontSize) = copy(xWidth = v)

  /*@param yHeight  height of the y axis in relative font size units */
  def withYHeight(v: RelFontSize) = copy(yHeight = v)

  /*@param frame  Draw a rectangular frame around the plot area */
  def withFrame(v: Boolean) = copy(frame = v)

  /*@param xLabelRotation  x axis label rotation */
  def withXLabelRotation(v: Double) = copy(xLabelRotation = v)

  /*@param yLabelRotation  y axis label rotation */
  def withYLabelRotation(v: Double) = copy(yLabelRotation = v)

  /*@param origin
   *   World/Axis coordinate of where the two axes will intersect. Data driven
   *   if empty.
   */
  def withOrigin(v: Option[(Double, Double)]) = copy(origin = v)

  /*@param xCustomGrid  custom grid lines on x axis */
  def withXCustomGrid(v: Boolean) = copy(xCustomGrid = v)

  /*@param yCustomGrid  custom grid line son y axis */
  def withYCustomGrid(v: Boolean) = copy(yCustomGrid = v)

  /*@param legendLayout  Layout of the legend items */
  def withLegendLayout(v: Layout) = copy(legendLayout = v)

  /*@param legendDistance
   *   Distance of the legend items from other legend items
   */
  def withLegendDistance(v: RelFontSize) = copy(legendDistance = v)

  /*@param xTickLength  tick length on x axis */
  def withXTickLength(v: RelFontSize) = copy(xTickLength = v)

  /*@param yTickLength  tick length on y axis */
  def withYTickLength(v: RelFontSize) = copy(yTickLength = v)

  /*@param xLineWidthFraction
   *   The length of the line visually representing the x axis as a fraction of
   *   the logical range of the axis
   */
  def withXLineWidthFraction(v: Double) = copy(xLineWidthFraction = v)

  /*@param yLineWidthFraction
   *   The length of the line visually representing the y axis as a fraction of
   *   the logical range of the axis
   */
  def withYLineWidthFraction(v: Double) = copy(yLineWidthFraction = v)

  /*@param xLineStartFraction
   *   The start of the line which represents the x axis
   */
  def withXLineStartFraction(v: Double) = copy(xLineStartFraction = v)

  /*@param yLineStartFraction
   *   The start of the line which represents the x axis
   */
  def withYLineStartFraction(v: Double) = copy(yLineStartFraction = v)

  /*@param topPadding
   *   padding around the whole plot (includes xlab, ylab, main labels)
   */
  def withTopPadding(v: RelFontSize) = copy(topPadding = v)

  /*@param bottomPadding  padding around the whole plot */
  def withBottomPadding(v: RelFontSize) = copy(bottomPadding = v)

  /*@param leftPadding  padding around the whole plot */
  def withLeftPadding(v: RelFontSize) = copy(leftPadding = v)

  /*@param rightPadding  padding around the whole plot */
  def withRightPadding(v: RelFontSize) = copy(rightPadding = v)

  /*@param xLabDistance  xlab to axis distance */
  def withXLabDistance(v: RelFontSize) = copy(xLabDistance = v)

  /*@param yLabDistance  ylab to axis distance */
  def withYLabDistance(v: RelFontSize) = copy(yLabDistance = v)

  /*@param mainLabDistance  main to frame distance */
  def withMainLabDistance(v: RelFontSize) = copy(mainLabDistance = v)

  /*@param xTickSpace
   *   x axis tick spacing, automatic if empty, recommended empty
   */
  def withXTickSpace(v: Option[Double]) = copy(xTickSpace = v)

  /*@param yTickSpace
   *   y axis tick spacing, automatic if empty, recommended empty
   */
  def withYTickSpace(v: Option[Double]) = copy(yTickSpace = v)

  /*@param noLegend  skip drawing legend */
  def withNoLegend(v: Boolean) = copy(noLegend = v)

  /*@param xNoTickLabel  skip drawing tick labels on x axis */
  def withXNoTickLabel(v: Boolean) = copy(xNoTickLabel = v)

  /*@param yNoTickLabel  skip drawing tick labels on y axis */
  def withYNoTickLabel(v: Boolean) = copy(yNoTickLabel = v)

}

object Parameters {
  @scala.annotation.nowarn
  private def unapply(p: Parameters) = p

  def apply() = new Parameters(
    noLegend = false,
    yTickSpace = None,
    xTickSpace = None,
    mainLabDistance = 0.75 fts,
    yLabDistance = 0.5 fts,
    xLabDistance = 0.5 fts,
    rightPadding = 5d fts,
    leftPadding = 5d fts,
    bottomPadding = 3d fts,
    topPadding = 3d fts,
    yLineStartFraction = 0d,
    xLineStartFraction = 0d,
    yLineWidthFraction = 1d,
    xLineWidthFraction = 1d,
    yTickLength = 0.4 fts,
    xTickLength = 0.4 fts,
    legendDistance = 0.5 fts,
    legendLayout = ColumnLayout(
      numRows = 10,
      horizontalGap = 0.75 fts,
      verticalGap = 0.4 fts
    ),
    yCustomGrid = false,
    xCustomGrid = false,
    origin = None,
    yLabelRotation = 0d,
    xLabelRotation = 0d,
    frame = true,
    yHeight = 20 fts,
    xWidth = 20 fts,
    ygrid = true,
    xgrid = true,
    legendWidth = 30 fts,
    legendFontSize = 1 fts,
    yAxisMargin = 0.05,
    xAxisMargin = 0.05,
    yNumTicks = 6,
    xNumTicks = 6,
    mainFontSize = 1 fts,
    yLabFontSize = 1 fts,
    xLabFontSize = 1 fts,
    extraLegend = Nil,
    draw1Line = false,
    ylim = None,
    xlim = None,
    ynames = Nil,
    xnames = Nil,
    ylab = "",
    xlab = "",
    main = "",
    ylog = false,
    xlog = false,
    xNoTickLabel = false,
    yNoTickLabel = false
  )
}
