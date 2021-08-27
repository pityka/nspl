package org.nspl

import data._
import scala.util.Try

trait DataRenderer {
  def render[R <: RenderingContext](
      data: Row,
      xAxis: Axis,
      yAxis: Axis,
      ctx: R,
      tx: AffineTransform
  )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit
  def asLegend: Option[LegendElem]
  def clear[R <: RenderingContext](ctx: R)(implicit
      re: Renderer[ShapeElem, R],
      rt: Renderer[TextBox, R]
  ): Unit = ()
  def xMinMax(ds: DataSource): Option[MinMax]
  def yMinMax(ds: DataSource): Option[MinMax]
}

trait Renderers {

  def point[T: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      colorCol: Int = 2,
      sizeCol: Int = 3,
      shapeCol: Int = 4,
      errorTopCol: Int = 5,
      errorBottomCol: Int = 6,
      size: Double = 3d,
      color: Colormap = DiscreteColors(14),
      shapes: Vector[Shape] = shapeList,
      pointSizeIsInDataSpaceUnits: Boolean = false,
      keepPointShapeAspectRatio: Boolean = false,
      valueText: Boolean = false,
      labelText: Boolean = false,
      labelFontSize: RelFontSize = 0.4 fts,
      labelColor: Color = Color.black,
      errorBarStroke: StrokeConf = StrokeConf(lineWidth),
      transparent: Option[Double] = None
  ) = new DataRenderer {

    def asLegend = Some(PointLegend(shapes.head, color(0)))

    def xMinMax(ds: DataSource) = Try(ds.columnMinMax(xCol)).toOption
    def yMinMax(ds: DataSource) = Try(ds.columnMinMax(yCol)).toOption

    val shapesAndTextLabels =
      scala.collection.mutable
        .ArrayBuffer[(ShapeElem, TextBox, AffineTransform)]()

    override def clear[R <: RenderingContext](ctx: R)(implicit
        re: Renderer[ShapeElem, R],
        rt: Renderer[TextBox, R]
    ) = {
      val labelLayout =
        LabelLayout
          .many(shapesAndTextLabels.map(v => (v._1.bounds, v._2.bounds)))
      labelLayout.zip(shapesAndTextLabels).foreach {
        case ((updatedBound, connectionLine), (_, label, tx)) =>
          val labelUpdated = label
            .translate(
              updatedBound.x - label.bounds.x,
              updatedBound.y - label.bounds.y
            )
            .transform(_ => tx)

          rt.render(ctx, labelUpdated)
          connectionLine match {
            case None                                   =>
            case Some((p1, p2)) if p1.distance(p2) <= 0 =>
            case Some((p1, p2)) =>
              val lineElem = ShapeElem(
                Shape.line(p1, p2),
                strokeColor = labelColor,
                stroke = Some(Stroke(lineWidth.value * 0.3))
              ).transform(_ => tx)
              re.render(ctx, lineElem)
          }

      }
    }

    def render[R <: RenderingContext](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      if (data.dimension > xCol && data.dimension > yCol) {
        val wX = data(xCol)
        val wY = data(yCol)

        if (
          wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max
        ) {
          val dataColorValue =
            if (data.dimension > colorCol) data(colorCol) else 0d

          val skip =
            (transparent.isDefined && transparent.get == dataColorValue)

          val color1 = color(dataColorValue)
          val shape =
            if (data.dimension > shapeCol)
              shapes(data(shapeCol).toInt % shapes.size)
            else shapes.head
          val size1 = if (data.dimension > sizeCol) data(sizeCol) else size

          val unitWidthX = if (pointSizeIsInDataSpaceUnits) {
            math.abs(xAxis.worldToView(0) - xAxis.worldToView(1))
          } else 1d
          val unitWidthY = if (pointSizeIsInDataSpaceUnits) {
            math.abs(yAxis.worldToView(0) - yAxis.worldToView(1))
          } else 1d

          val shapeBounds = shape.bounds

          val shapeAspectRatio =
            if (keepPointShapeAspectRatio) shapeBounds.h / shapeBounds.w
            else 1d
          val factorX = unitWidthX * size1 / shapeBounds.w
          val factorY = unitWidthY * size1 * shapeAspectRatio / shapeBounds.h

          val vX = xAxis.worldToView(wX)
          val vY = yAxis.worldToView(wY)
          val shape1PreTransform: ShapeElem = ShapeElem(
            shape.transform(_ =>
              AffineTransform
                .translate(vX, vY)
                .concatScale(factorX, factorY)
            ),
            fill = color1
          )

          val shape1 = shape1PreTransform.transform(b => tx)

          if (!skip) {
            re.render(ctx, shape1)
          }

          if (valueText) {
            val tbPreTransform = TextBox(
              f"${data(colorCol)}%.2g",
              color = labelColor,
              fontSize = labelFontSize
            ).translate(vX, vY)
              .transform(b =>
                AffineTransform
                  .translate(0, -1 * b.h - shape.bounds.h * factorY * 0.5)
              )

            shapesAndTextLabels += ((shape1PreTransform, tbPreTransform, tx))

          }

          if (labelText) {
            val tbPreTransform = TextBox(
              data.label,
              color = labelColor,
              fontSize = labelFontSize
            ).translate(vX, vY)
              .transform(b =>
                AffineTransform.translate(
                  -0.2 * b.w,
                  -1 * b.h - shape.bounds.h * factorY * 0.5
                )
              )

            shapesAndTextLabels += ((shape1PreTransform, tbPreTransform, tx))

          }

          if (data.dimension > errorTopCol) {
            val errorTop = data(errorTopCol)
            val shape1: ShapeElem = ShapeElem(
              Shape.line(
                Point(vX, vY),
                Point(vX, yAxis.worldToView(wY - errorTop))
              ),
              stroke = Some(errorBarStroke.value)
            ).transform(_ => tx)
            re.render(ctx, shape1)
          }
          if (data.dimension > errorBottomCol) {
            val errorTop = data(errorBottomCol)
            val shape1: ShapeElem = ShapeElem(
              Shape.line(
                Point(vX, vY),
                Point(vX, yAxis.worldToView(wY + errorTop))
              ),
              stroke = Some(errorBarStroke.value)
            ).transform(_ => tx)
            re.render(ctx, shape1)
          }
        }
      } else
        throw new RuntimeException(
          s"Record has no X or Y elements. size: ${data.dimension} vs idx $xCol $yCol"
        )
    }
  }

  def line[F: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      colorCol: Int = 2,
      stroke: StrokeConf = StrokeConf(lineWidth),
      color: Colormap = Color.black
  ) = new DataRenderer {

    def xMinMax(ds: DataSource) = Some(ds.columnMinMax(xCol))
    def yMinMax(ds: DataSource) = Some(ds.columnMinMax(yCol))

    var currentPoint: Option[Point] = None
    def asLegend = Some(LineLegend(stroke.value, color(0)))

    override def clear[R <: RenderingContext](ctx: R)(implicit
        re: Renderer[ShapeElem, R],
        rt: Renderer[TextBox, R]
    ): Unit = {
      currentPoint = None
    }

    def render[R <: RenderingContext](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol)
      val wY = data(yCol)

      if (
        wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max
      ) {

        val color1 =
          if (data.dimension > colorCol) color(data(colorCol)) else color(0d)

        val vX = xAxis.worldToView(wX)
        val vY = yAxis.worldToView(wY)

        val p = Point(vX, vY)

        if (currentPoint.isEmpty) {
          currentPoint = Some(p)
        } else {

          val shape1 = ShapeElem(
            Shape.line(currentPoint.get, p),
            fill = Color.transparent,
            strokeColor = color1,
            stroke = Some(stroke.value.copy(cap = CapRound))
          ).transform(_ => tx)

          re.render(ctx, shape1)

          currentPoint = Some(p)

        }
      }
    }
  }

  /** Paints the area between the (x,y) and (x,0) or between (x,y) and (x,y2) if
    * y2 is present
    */
  def area(
      xCol: Int = 0,
      yCol: Int = 1,
      colorCol: Int = 2,
      yCol2: Option[Int] = None,
      color: Colormap = Color.black
  ) = new DataRenderer {
    var currentPoint1: Option[Point] = None
    var currentPoint2: Option[Point] = None
    def asLegend = Some(PointLegend(shapeList(1), color(0)))

    override def clear[R <: RenderingContext](ctx: R)(implicit
        re: Renderer[ShapeElem, R],
        rt: Renderer[TextBox, R]
    ): Unit = {
      currentPoint2 = None
      currentPoint1 = None
    }

    def xMinMax(ds: DataSource) = Some(ds.columnMinMax(xCol))
    def yMinMax(ds: DataSource) = {
      val max = ds.columnMinMax(yCol).max
      val min = yCol2.map(y => ds.columnMinMax(y).min).getOrElse(0d)
      Some(MinMaxImpl(min, max))
    }

    def render[R <: RenderingContext](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol)
      val wY = data(yCol)

      if (
        wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max
      ) {

        val color1 =
          if (data.dimension > colorCol) color(data(colorCol)) else color(0d)

        val wYBottom = yCol2.map { i =>
          val w = data(i)
          if (w > yAxis.max) yAxis.max
          else if (w < yAxis.min) yAxis.min
          else w
        }

        val vX = xAxis.worldToView(wX)
        val vY = yAxis.worldToView(wY)

        val p1 = Point(vX, vY)
        val p2 = Point(
          vX,
          wYBottom
            .map(w => yAxis.worldToView(w))
            .getOrElse(yAxis.worldToView(yAxis.min))
        )

        if (currentPoint1.isEmpty) {
          currentPoint1 = Some(p1)
          currentPoint2 = Some(p2)
        } else {

          val shape = SimplePath(
            List(
              Point(currentPoint1.get.x - .5, currentPoint1.get.y),
              Point(p1.x, p1.y),
              Point(p2.x, p2.y),
              Point(currentPoint2.get.x - .5, currentPoint2.get.y)
            )
          )

          val shape1 = ShapeElem(
            shape,
            fill = color1
          ).transform(_ => tx)

          re.render(ctx, shape1)

          currentPoint1 = Some(p1)
          currentPoint2 = Some(p2)

        }
      }
    }
  }

  def polynom(
      renderer: () => DataRenderer
  ) = new DataRenderer {
    def asLegend = renderer().asLegend

    def evaluatePolynomial(coef: Array[Double], x: Double) = {
      var p = 0.0;
      var i = coef.length - 1
      while (i >= 0) {
        p = coef(i) + x * p
        i = i - 1
      }
      p
    }

    def xMinMax(ds: DataSource): Option[MinMax] = None
    def yMinMax(ds: DataSource): Option[MinMax] = None

    def render[R <: RenderingContext](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val r = renderer()

      0 until 1000 foreach { i =>
        val x = xAxis.min + i * (xAxis.max - xAxis.min) / 1000.0
        val y = evaluatePolynomial(data.allColumns.toArray, x)
        r.render(VectorRow(Vector(x, y), ""), xAxis, yAxis, ctx, tx)
      }

    }
  }

  def bar[F: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      fillCol: Int = 2,
      horizontal: Boolean = false,
      stroke: StrokeConf = StrokeConf(lineWidth),
      strokeColor: Color = Color.black,
      fill: Colormap = Color.white,
      width: Double = 1,
      yCol2: Option[Int] = None,
      widthCol: Int = 3
  ) = new DataRenderer {

    def xMinMax(ds: DataSource): Option[MinMax] = Some(ds.columnMinMax(xCol))
    def yMinMax(ds: DataSource): Option[MinMax] = Some(ds.columnMinMax(yCol))

    def asLegend = Some(PointLegend(shapeList(1), fill(0)))
    def render[R <: RenderingContext](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol)
      val wY = data(yCol)

      if (
        wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max
      ) {

        val color1 =
          if (data.dimension > fillCol) fill(data(fillCol)) else fill(0d)
        val width1 = if (data.dimension > widthCol) data(widthCol) else width

        if (!horizontal) {

          val wYBottom = yCol2
            .map { i =>
              val w = data(i)
              if (w > yAxis.max) yAxis.max
              else if (w < yAxis.min) yAxis.min
              else w
            }
            .getOrElse(
              if (0d > yAxis.max) yAxis.max
              else if (0d < yAxis.min) yAxis.min
              else 0d
            )

          val vX = xAxis.worldToView(wX)
          val vXMin = xAxis.worldToView(xAxis.min)
          val vXMax = xAxis.worldToView(xAxis.max)
          val vWidth1 =
            math.abs(xAxis.worldToView(0.0) - xAxis.worldToView(width1))

          val outOfBoundsLeft = math.max(0d, vXMin - (vX - vWidth1 * 0.5))
          val outOfBoundsRight = math.max(0d, vX + vWidth1 * 0.5 - vXMax)

          val vWidth = vWidth1 - outOfBoundsLeft - outOfBoundsRight

          val vY2 = yAxis.worldToView(wYBottom)
          val vY = yAxis.worldToView(wY)

          val vHeight = math.abs(vY2 - vY)

          val rectangle =
            if (vY2 > vY)
              Shape.rectangle(
                vX - vWidth1 * 0.5 + outOfBoundsLeft,
                vY,
                vWidth,
                vHeight
              )
            else
              Shape.rectangle(
                vX - vWidth1 * 0.5 + outOfBoundsLeft,
                vY2,
                vWidth,
                vHeight
              )

          val shape1 = ShapeElem(
            rectangle,
            fill = color1,
            stroke = Some(stroke.value),
            strokeColor = strokeColor
          ).transform(_ => tx)

          re.render(ctx, shape1)

        } else {

          val wXBottom = yCol2
            .map { i =>
              val w = data(i)
              if (w > xAxis.max) xAxis.max
              else if (w < xAxis.min) xAxis.min
              else w
            }
            .getOrElse(
              if (0d > xAxis.max) xAxis.max
              else if (0d < xAxis.min) xAxis.min
              else 0d
            )

          val vY = yAxis.worldToView(wY)
          val vYMin = yAxis.worldToView(yAxis.min)
          val vYMax = yAxis.worldToView(yAxis.max)

          val vWidth1 =
            math.abs(yAxis.worldToView(0.0) - yAxis.worldToView(width))

          val outOfBoundsTop = math.max(0d, vYMax - (vY - vWidth1 * 0.5))
          val outOfBoundsBottom = math.max(0d, vY + vWidth1 * 0.5 - vYMin)

          val vWidth = vWidth1 - outOfBoundsTop - outOfBoundsBottom

          val vX = xAxis.worldToView(wX)
          val vX2 = xAxis.worldToView(wXBottom)
          val vHeight = math.abs(vX2 - vX)

          val rectangle =
            if (wX > 0)
              Shape.rectangle(
                vX2,
                vY - vWidth1 * 0.5 + outOfBoundsTop,
                vHeight,
                vWidth
              )
            else
              Shape.rectangle(
                vX,
                vY - vWidth1 * 0.5 + outOfBoundsTop,
                vHeight,
                vWidth
              )

          val shape1 = ShapeElem(
            rectangle,
            fill = color1,
            stroke = Some(stroke.value),
            strokeColor = strokeColor
          ).transform(_ => tx)

          re.render(ctx, shape1)

        }

      }
    }
  }

  def abline(
      a: Double,
      b: Double,
      renderer: DataRenderer
  ) =
    (dataSourceFromRows(Seq(a -> b)), List(renderer))

  def boxwhisker[F: FC](
      xCol: Int = 0,
      medianCol: Int = 1,
      q1Col: Int = 2,
      q3Col: Int = 3,
      minCol: Int = 4,
      maxCol: Int = 5,
      x2Col: Int = 6,
      fillCol: Int = 7,
      width: Double = 1,
      stroke: StrokeConf = StrokeConf(lineWidth),
      strokeColor: Color = Color.black,
      fill: Colormap = Color.white
  ) = new DataRenderer {
    def asLegend = Some(PointLegend(shapeList(1), fill(0)))

    def xMinMax(ds: DataSource): Option[MinMax] = Some(ds.columnMinMax(xCol))
    def yMinMax(ds: DataSource): Option[MinMax] = {
      val min = ds.columnMinMax(minCol).min
      val max = ds.columnMinMax(maxCol).max
      Some(MinMaxImpl(min, max))
    }

    def render[R <: RenderingContext](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX1 = data(xCol)
      val q2 = data(medianCol)
      val q1 = data(q1Col)
      val q3 = data(q3Col)
      val min = data(minCol)
      val max = data(maxCol)
      val color1 =
        if (data.dimension > fillCol) fill(data(fillCol)) else fill(0d)
      val width1 = if (data.dimension > x2Col) data(x2Col) - wX1 else width
      val wX = wX1 + .5 * width1

      if (wX >= xAxis.min && wX <= xAxis.max) {

        val vWidth =
          math.abs(xAxis.worldToView(0.0) - xAxis.worldToView(width1))
        val vX = xAxis.worldToView(wX)
        val vQ1 = yAxis.worldToView(q1)
        val vQ2 = yAxis.worldToView(q2)
        val vQ3 = yAxis.worldToView(q3)
        val vMin = yAxis.worldToView(min)
        val vMax = yAxis.worldToView(max)
        val vHeight = math.abs(vQ1 - vQ3)

        val shape1 = ShapeElem(
          Shape.rectangle(vX - vWidth * 0.5, vQ3, vWidth, vHeight),
          fill = color1,
          stroke = Some(stroke.value),
          strokeColor = strokeColor
        ).transform(_ => tx)

        re.render(ctx, shape1)

        val shape2 = ShapeElem(
          Shape
            .line(Point(vX - vWidth * 0.5, vQ2), Point(vX + vWidth * 0.5, vQ2)),
          fill = color1,
          stroke = Some(stroke.value),
          strokeColor = strokeColor
        ).transform(_ => tx)

        re.render(ctx, shape2)

        val shape3 = ShapeElem(
          Shape.line(Point(vX, vQ1), Point(vX, vMin)),
          fill = color1,
          stroke = Some(stroke.value),
          strokeColor = strokeColor
        ).transform(_ => tx)

        re.render(ctx, shape3)

        val shape4 = ShapeElem(
          Shape.line(Point(vX, vQ3), Point(vX, vMax)),
          fill = color1,
          stroke = Some(stroke.value),
          strokeColor = strokeColor
        ).transform(_ => tx)

        re.render(ctx, shape4)

      }
    }
  }

  def lineSegment[F: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      x2Col: Int = 2,
      y2Col: Int = 3,
      colorCol: Int = 4,
      stroke: StrokeConf = StrokeConf(lineWidth, CapRound),
      color: Colormap = HeatMapColors(0, 1)
  ) = new DataRenderer {

    def xMinMax(ds: DataSource): Option[MinMax] = {
      val m1 = ds.columnMinMax(xCol)
      val m2 = ds.columnMinMax(x2Col)
      Some(MinMaxImpl(math.min(m1.min, m2.min), math.max(m1.max, m2.max)))
    }

    def yMinMax(ds: DataSource): Option[MinMax] = {
      val m1 = ds.columnMinMax(yCol)
      val m2 = ds.columnMinMax(y2Col)
      Some(MinMaxImpl(math.min(m1.min, m2.min), math.max(m1.max, m2.max)))
    }

    def asLegend = Some(LineLegend(stroke.value, color(0)))

    def render[R <: RenderingContext](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol)
      val wY = data(yCol)
      val wX2 = data(x2Col)
      val wY2 = data(y2Col)

      if (
        wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max
      ) {

        val color1 =
          if (data.dimension > colorCol) color(data(colorCol)) else color(0d)

        val vX = xAxis.worldToView(wX)
        val vY = yAxis.worldToView(wY)

        val vX2 = xAxis.worldToView(wX2)
        val vY2 = yAxis.worldToView(wY2)

        val p = Point(vX, vY)
        val p2 = Point(vX2, vY2)

        val shape1 = ShapeElem(
          Shape.line(p, p2),
          strokeColor = color1,
          stroke = Some(stroke.value)
        ).transform(_ => tx)

        re.render(ctx, shape1)

      }
    }
  }

}
