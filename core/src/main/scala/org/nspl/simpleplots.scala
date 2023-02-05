package org.nspl

import data._

sealed trait LegendConfig
case object NotInLegend extends LegendConfig
case class InLegend(text: String) extends LegendConfig

/* Factory methods for common plots. */
private[nspl] trait SimplePlots {


  /** Factory method to create the scene graph of a plot in a Cartesian
    * coordinate system.
    *
    * Example a plot of a scatter plot of dots connected with lines:
    *
    * ```scala
    * val someData =
    * 0 until 10 map (i => (i.toDouble, (i * i).toDouble))
    *
    * val plot = xyplot((someData, List(point(),line()),InLegend("some"))(
    *         par(
    *           main="Main label",
    *           xlab="x axis label",
    *           ylab="y axis label"
    *         )
    *       )
    * ```
    *
    * @param data
    *   Triplets of data source, data renderers, legend configuration. Each
    *   triplet describes what data (DataSource) to be drawn and how to
    *   represent them (by the DataRenderers), and whether to include them in
    *   the legend. A single data source may have multiple visual
    *   representations e.g. a point and a line.
    *
    * @param parameters
    *   Generic characteristics of the plot. see [[org.nspl.par.apply]].
    */
  def xyplot[F: FC](data: (DataSource, List[DataRenderer], LegendConfig)*)(
      parameters: Parameters = par
  ) = {
    import parameters._
    val xFac = if (xlog) Log10AxisFactory else LinearAxisFactory
    val yFac = if (ylog) Log10AxisFactory else LinearAxisFactory

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
        entries = ((data
          .map { case (_, render, conf) =>
            conf match {
              case NotInLegend => None
              case InLegend(name1) =>
                Some(name1 -> render.flatMap(_.asLegend.toList))
            }
          }
          .collect { case Some(x) => x }) ++ extraLegend.map(v =>
          (v._1, List(v._2))
        )).toList.distinct,
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
        mainDistance = mainLabDistance,
        xNoTickLabel = xNoTickLabel,
        yNoTickLabel = yNoTickLabel
      )

    group(
      plotArea,
      legend1,
      HorizontalStack(Align.Anchor, legendDistance)
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
            data
              .map { case (_, render, conf) =>
                conf match {
                  case NotInLegend => None
                  case InLegend(name) =>
                    Some(
                      name -> render.flatMap(_.asLegend.toList)
                    )
                }
              }
              .collect { case Some(x) => x }
          ) ++ extraLegend.map { case (name, legend) => (name, List(legend)) }
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
      HorizontalStack(Align.Anchor, legendDistance)
    )
  }

  def stackedBarPlot[F: FC](
      data: DataSource,
      legend: Seq[(Int, String, Colormap)],
      xCol: Int = 0,
      relative: Boolean = false
  )(parameters: Parameters) = {
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
        legend1.zip(data1).map { case ((_, label, color), data) =>
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
        parameters
      )

    }
  }

  def boxplot[F: FC](
      data: DataSourceWithQuantiles,
      xnames: Seq[String] = Nil,
      boxColor: Colormap = Color.gray4,
      boxWidth: Double = 1d
  )(parameters: Parameters) = {

    val bxdata = boxplotData(data)

    boxplotImpl(
      bxdata,
      xnames,
      boxColor,
      boxWidth
    )(parameters)
  }

  def boxplotImpl[F: FC](
      bxdata: DataSource,
      xnames: Seq[String],
      boxColor: Colormap = Color.gray4,
      boxWidth: Double = 1d
  )(parameters: Parameters) = {
    import parameters.{xnames => _, _}
    val min = bxdata.iterator.flatMap(_.allColumns.iterator.drop(1).take(5)).min
    val max = bxdata.iterator.flatMap(_.allColumns.iterator.drop(1).take(5)).max

    val xnames1 =
      if (xnames.isEmpty)
        bxdata.iterator.toList
          .map(ds => (ds(0), ds.label))
          .filter(_._2.nonEmpty)
      else xnames.zipWithIndex.map(x => x._2.toDouble -> x._1)

    xyplotareaBuild(
      List(bxdata -> List(boxwhisker(fill = boxColor, width = boxWidth))),
      AxisSettings(
        LinearAxisFactory,
        customTicks = xnames1,
        numTicks = if (xnames1.isEmpty) 5 else 0,
        fontSize = xLabFontSize,
        width = xWidth,
        labelRotation = xLabelRotation,
        tickLength = xTickLength,
        lineLengthFraction = xLineWidthFraction,
        lineStartFraction = xLineStartFraction,
        tickSpace = xTickSpace
      ),
      AxisSettings(
        LinearAxisFactory,
        fontSize = yLabFontSize,
        width = yHeight,
        labelRotation = yLabelRotation,
        tickLength = yTickLength,
        lineLengthFraction = yLineWidthFraction,
        lineStartFraction = yLineStartFraction,
        tickSpace = yTickSpace
      ),
      None,
      xlim = None,
      ylim = parameters.ylim.orElse(Some(min -> max)),
      xgrid = xgrid,
      ygrid = ygrid,
      frame = frame,
      main = main,
      xlab = xlab,
      ylab = ylab,
      xlabFontSize = xLabFontSize,
      ylabFontSize = yLabFontSize,
      mainFontSize = mainFontSize
    )
  }

  def boxplotFromLabels[T: Ordering, F: FC](
      data: Seq[(T, Double)],
      boxColor: Colormap = Color.gray4,
      useLabels: Boolean = true
  )(parameters: Parameters) = {
    val bxdata = boxplotData(data.toSeq)

    boxplotImpl(
      bxdata,
      if (useLabels) bxdata.iterator.map(_.label).toList else Nil,
      boxColor
    )(parameters)
  }

  def binnedboxplot[F: FC](
      dim1: Seq[Double],
      dim2: Seq[Double],
      bins: Int = 10,
      boxColor: Colormap = Color.gray4,
      xgrid: Boolean = false
  )(parameters: Parameters) = {
    val overriden = parameters.copy(
      xgrid = xgrid,
      xlim = Some(dim2.min -> dim2.max),
      ylim = Some(dim1.min -> dim1.max)
    )

    xyplot(
      boxplotData(
        dim1,
        dim2,
        1 to bins map (i => i / bins.toDouble),
        Vector.fill(bins)(0d)
      ) -> boxwhisker(fill = boxColor)
    )(
      overriden
    )
  }

  def contourplot[F: FC](
      xlim: (Double, Double),
      ylim: (Double, Double),
      f: (Double, Double) => Double,
      n: Int,
      levels: Int
  )(parameters: Parameters) = {
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
      parameters
    )
  }

  def rasterplot[F: FC](
      data: DataSource,
      colormap: Colormap = HeatMapColors(0, 1),
      xCol: Int = 0,
      yCol: Int = 1,
      zCol: Int = 2,
      valueText: Boolean = false,
      valueColor: Color = Color.black,
      valueFontSize: RelFontSize = 0.4 fts,
      zlim: Option[(Double, Double)] = None,
      transparentPixels: Option[Double] = None,
      zlab: String = "",
      zNumTicks: Int = 2
  )(parameters: Parameters) = {
    import parameters._
    val minmaxx = data.columnMinMax(xCol)
    val minmaxy = data.columnMinMax(yCol)
    val minmaxz = data.columnMinMax(zCol)
    val xmin = minmaxx.map(_.min).getOrElse(0d)
    val xmax = minmaxx.map(_.max).getOrElse(1d)
    val ymin = minmaxy.map(_.min).getOrElse(0d)
    val ymax = minmaxy.map(_.max).getOrElse(1d)
    val zmin = zlim.map(_._1).orElse(minmaxz.map(_.min)).getOrElse(0d)
    val zmax = {
      zlim
        .map(_._2)
        .orElse(minmaxz.map(_.max))
        .map { v =>
          if (v == zmin) zmin + 1
          else v
        }
        .getOrElse(1d)
    }

    val eitherLegend =
      if (noLegend || extraLegend.nonEmpty)
        scala.util.Left(
          legend(
            entries =
              if (noLegend) Nil
              else extraLegend.toList.map(v => (v._1 -> List(v._2))),
            fontSize = legendFontSize,
            width = legendWidth,
            legendLayout
          )
        )
      else
        scala.util.Right(
          heatmapLegend(
            min = zmin,
            max = zmax,
            color = colormap,
            labelText = zlab,
            numTicks = zNumTicks,
            width = 10 fts,
            height = 1 fts,
            fontSize = 1.0 fts
          )
        )

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
          fontSize = xLabFontSize,
          numTicks = xNumTicks,
          tickSpace = xTickSpace,
          numMinorTicksFactor = 0,
          tickLength = xTickLength,
          labelRotation = -0.5 * math.Pi,
          width = xWidth
        ),
        AxisSettings(
          LinearAxisFactory,
          customTicks = ynames,
          fontSize = yLabFontSize,
          numTicks = yNumTicks,
          tickSpace = yTickSpace,
          numMinorTicksFactor = 0,
          tickLength = yTickLength,
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
        xlabFontSize = xLabFontSize,
        ylabFontSize = yLabFontSize,
        mainFontSize = mainFontSize,
        xNoTickLabel = xNoTickLabel,
        yNoTickLabel = yNoTickLabel,
        xgrid = xgrid,
        ygrid = ygrid
      ),
      ElemEither(eitherLegend),
      HorizontalStack(Align.Anchor, 1d fts)
    )

  }
}
