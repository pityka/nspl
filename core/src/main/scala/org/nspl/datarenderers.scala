package org.nspl

import data._

trait DataRenderer {
  def render[R <: RenderingContext](data: Row, xAxis: Axis, yAxis: Axis, ctx: R, tx: AffineTransform)(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit
  def asLegend: Option[LegendElem]
  def clear: Unit = ()
}

trait Renderers {

  def point(
    xCol: Int = 0,
    yCol: Int = 1,
    colorCol: Int = 2,
    sizeCol: Int = 3,
    shapeCol: Int = 4,
    errorTopCol: Int = 5,
    errorBottomCol: Int = 6,
    size: Double = 3.0,
    color: Colormap = DiscreteColors(14),
    shapes: Vector[Shape] = shapeList,
    pointSizeIsInDataSpaceUnits: Boolean = false,
    valueText: Boolean = false,
    labelText: Boolean = false,
    labelFontSize: RelFontSize = 0.4 fts,
    labelColor: Color = Color.black,
    errorBarStroke: Stroke = Stroke(1d)
  ) = new DataRenderer {

    def asLegend = Some(PointLegend(shapes.head, color(0)))

    def render[R <: RenderingContext](data: Row, xAxis: Axis, yAxis: Axis, ctx: R, tx: AffineTransform)(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      if (data.dimension > xCol && data.dimension > yCol) {
        val wX = data(xCol)
        val wY = data(yCol)

        if (wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max) {
          val color1 = if (data.dimension > colorCol) color(data(colorCol)) else color(0d)
          val shape = if (data.dimension > shapeCol) shapes(data(shapeCol).toInt % shapes.size) else shapes.head
          val size1 = if (data.dimension > sizeCol) data(sizeCol) else size

          val unitWidthX = if (pointSizeIsInDataSpaceUnits) {
            math.abs(xAxis.worldToView(0) - xAxis.worldToView(1))
          } else 1d
          val unitWidthY = if (pointSizeIsInDataSpaceUnits) {
            math.abs(yAxis.worldToView(0) - yAxis.worldToView(1))
          } else 1d

          val factorX = unitWidthX * size1 / shape.bounds.w
          val factorY = unitWidthY * size1 / shape.bounds.h

          val vX = xAxis.worldToView(wX)
          val vY = yAxis.worldToView(wY)
          val shape1: ShapeElem = ShapeElem(
            shape,
            fill = color1
          )
            .scale(factorX, factorY)
            .translate(vX, vY)
            .transform(b => tx)

          re.render(ctx, shape1)

          if (valueText) {
            val tb = TextBox(
              f"${data(colorCol)}%.2g",
              color = labelColor,
              fontSize = labelFontSize
            )
              .translate(vX, vY)
              .transform(b => tx.concat(AffineTransform.translate(0, -1 * b.h)))

            rt.render(ctx, tb)
          }

          if (labelText) {
            val tb = TextBox(
              data.label,
              color = labelColor,
              fontSize = labelFontSize
            )
              .translate(vX, vY)
              .transform(b => tx.concat(AffineTransform.translate(0, -1 * b.h)))

            rt.render(ctx, tb)
          }

          if (data.dimension > errorTopCol) {
            val errorTop = data(errorTopCol)
            val shape1: ShapeElem = ShapeElem(
              Shape.line(Point(vX, vY), Point(vX, yAxis.worldToView(wY - errorTop))),
              stroke = Some(errorBarStroke)
            ).transform(_ => tx)
            re.render(ctx, shape1)
          }
          if (data.dimension > errorBottomCol) {
            val errorTop = data(errorBottomCol)
            val shape1: ShapeElem = ShapeElem(
              Shape.line(Point(vX, vY), Point(vX, yAxis.worldToView(wY + errorTop))),
              stroke = Some(errorBarStroke)
            ).transform(_ => tx)
            re.render(ctx, shape1)
          }
        }
      } else throw new RuntimeException(
        s"Record has no X or Y elements. size: ${data.dimension} vs idx $xCol $yCol"
      )
    }
  }

  def line(
    xCol: Int = 0,
    yCol: Int = 1,
    colorCol: Int = 2,
    stroke: Stroke = Stroke(1d),
    color: Colormap = Color.black
  ) = new DataRenderer {

    var currentPoint: Option[Point] = None
    def asLegend = Some(LineLegend(stroke, color(0)))
    override def clear = currentPoint = None

    def render[R <: RenderingContext](
      data: Row,
      xAxis: Axis,
      yAxis: Axis,
      ctx: R,
      tx: AffineTransform
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol)
      val wY = data(yCol)

      if (wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max) {

        val color1 = if (data.dimension > colorCol) color(data(colorCol)) else color(0d)

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
            stroke = Some(stroke.copy(cap = CapRound))
          ).transform(_ => tx)

          re.render(ctx, shape1)

          currentPoint = Some(p)

        }
      }
    }
  }

  /* Paints the area between the (x,y) and (x,0) or
   *  between (x,y) and (x,y2) if y2 is present */
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

    override def clear {
      currentPoint2 = None
      currentPoint1 = None
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

      if (wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max) {

        val color1 = if (data.dimension > colorCol) color(data(colorCol)) else color(0d)

        val wYBottom = yCol2.map { i =>
          val w = data(i)
          if (w > yAxis.max) yAxis.max
          else if (w < yAxis.min) yAxis.min
          else w
        }

        val vX = xAxis.worldToView(wX)
        val vY = yAxis.worldToView(wY)

        val p1 = Point(vX, vY)
        val p2 = Point(vX, wYBottom.map(w =>
          yAxis.worldToView(w)).getOrElse(yAxis.worldToView(yAxis.min)))

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
    renderer: () => DataRenderer = () => line()
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

  def bar(
    xCol: Int = 0,
    yCol: Int = 1,
    fillCol: Int = 2,
    horizontal: Boolean = false,
    stroke: Stroke = Stroke(1d),
    strokeColor: Color = Color.black,
    fill: Colormap = Color.white,
    width: Double = 1d,
    yCol2: Option[Int] = None,
    widthCol: Int = 3
  ) = new DataRenderer {
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

      if (wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max) {

        val color1 = if (data.dimension > fillCol) fill(data(fillCol)) else fill(0d)
        val width1 = if (data.dimension > widthCol) data(widthCol) else width

        if (!horizontal) {

          val wYBottom = yCol2.map { i =>
            val w = data(i)
            if (w > yAxis.max) yAxis.max
            else if (w < yAxis.min) yAxis.min
            else w
          }.getOrElse(if (0d > yAxis.max) yAxis.max
          else if (0d < yAxis.min) yAxis.min
          else 0d)

          val vX = xAxis.worldToView(wX)
          val vXMin = xAxis.worldToView(xAxis.min)
          val vXMax = xAxis.worldToView(xAxis.max)
          val vWidth1 = math.abs(xAxis.worldToView(0.0) - xAxis.worldToView(width1))

          val outOfBoundsLeft = math.max(0d, vXMin - (vX - vWidth1 * 0.5))
          val outOfBoundsRight = math.max(0d, vX + vWidth1 * 0.5 - vXMax)

          val vWidth = vWidth1 - outOfBoundsLeft - outOfBoundsRight

          val vY2 = yAxis.worldToView(wYBottom)
          val vY = yAxis.worldToView(wY)

          val vHeight = math.abs(vY2 - vY)

          val rectangle = if (vY2 > vY) Shape.rectangle(vX - vWidth1 * 0.5 + outOfBoundsLeft, vY, vWidth, vHeight) else Shape.rectangle(vX - vWidth1 * 0.5 + outOfBoundsLeft, vY2, vWidth, vHeight)

          val shape1 = ShapeElem(
            rectangle,
            fill = color1,
            stroke = Some(stroke),
            strokeColor = strokeColor
          ).transform(_ => tx)

          re.render(ctx, shape1)

        } else {

          val wXBottom = yCol2.map { i =>
            val w = data(i)
            if (w > xAxis.max) xAxis.max
            else if (w < xAxis.min) xAxis.min
            else w
          }.getOrElse(if (0d > xAxis.max) xAxis.max
          else if (0d < xAxis.min) xAxis.min
          else 0d)

          val vY = yAxis.worldToView(wY)
          val vYMin = yAxis.worldToView(yAxis.min)
          val vYMax = yAxis.worldToView(yAxis.max)

          val vWidth1 = math.abs(yAxis.worldToView(0.0) - yAxis.worldToView(width))

          val outOfBoundsTop = math.max(0d, vYMax - (vY - vWidth1 * 0.5))
          val outOfBoundsBottom = math.max(0d, vY + vWidth1 * 0.5 - vYMin)

          val vWidth = vWidth1 - outOfBoundsTop - outOfBoundsBottom

          val vX = xAxis.worldToView(wX)
          val vX2 = xAxis.worldToView(wXBottom)
          val vHeight = math.abs(vX2 - vX)

          val rectangle = if (wX > 0) Shape.rectangle(vX2, vY - vWidth1 * 0.5 + outOfBoundsTop, vHeight, vWidth) else Shape.rectangle(vX, vY - vWidth1 * 0.5 + outOfBoundsTop, vHeight, vWidth)

          val shape1 = ShapeElem(
            rectangle,
            fill = color1,
            stroke = Some(stroke),
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
    renderer: DataRenderer = line()
  ) =
    (dataSourceFromRows(Seq(a -> b)), List(renderer))

  def boxwhisker(
    xCol: Int = 0,
    medianCol: Int = 1,
    q1Col: Int = 2,
    q3Col: Int = 3,
    minCol: Int = 4,
    maxCol: Int = 5,
    x2Col: Int = 6,
    fillCol: Int = 7,
    width: Double = 1,
    stroke: Stroke = Stroke(1d),
    strokeColor: Color = Color.black,
    fill: Colormap = Color.white
  ) = new DataRenderer {
    def asLegend = Some(PointLegend(shapeList(1), fill(0)))

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
      val color1 = if (data.dimension > fillCol) fill(data(fillCol)) else fill(0d)
      val width1 = if (data.dimension > x2Col) data(x2Col) - wX1 else width
      val wX = wX1 + .5 * width1

      if (wX >= xAxis.min && wX <= xAxis.max) {

        val vWidth = math.abs(xAxis.worldToView(0.0) - xAxis.worldToView(width1))
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
          stroke = Some(stroke),
          strokeColor = strokeColor
        ).transform(_ => tx)

        re.render(ctx, shape1)

        val shape2 = ShapeElem(
          Shape.line(Point(vX - vWidth * 0.5, vQ2), Point(vX + vWidth * 0.5, vQ2)),
          fill = color1,
          stroke = Some(stroke),
          strokeColor = strokeColor
        ).transform(_ => tx)

        re.render(ctx, shape2)

        val shape3 = ShapeElem(
          Shape.line(Point(vX, vQ1), Point(vX, vMin)),
          fill = color1,
          stroke = Some(stroke),
          strokeColor = strokeColor
        ).transform(_ => tx)

        re.render(ctx, shape3)

        val shape4 = ShapeElem(
          Shape.line(Point(vX, vQ3), Point(vX, vMax)),
          fill = color1,
          stroke = Some(stroke),
          strokeColor = strokeColor
        ).transform(_ => tx)

        re.render(ctx, shape4)

      }
    }
  }

  def lineSegment(
    xCol: Int = 0,
    yCol: Int = 1,
    x2Col: Int = 2,
    y2Col: Int = 3,
    colorCol: Int = 4,
    stroke: Stroke = Stroke(1d),
    color: Colormap = HeatMapColors(0, 1)
  ) = new DataRenderer {

    def asLegend = Some(LineLegend(stroke, color(0)))

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

      if (wX >= xAxis.min && wX <= xAxis.max && wY >= yAxis.min && wY <= yAxis.max) {

        val color1 = if (data.dimension > colorCol) color(data(colorCol)) else color(0d)

        val vX = xAxis.worldToView(wX)
        val vY = yAxis.worldToView(wY)

        val vX2 = xAxis.worldToView(wX2)
        val vY2 = yAxis.worldToView(wY2)

        val p = Point(vX, vY)
        val p2 = Point(vX2, vY2)

        val shape1 = ShapeElem(
          Shape.line(p, p2),
          strokeColor = color1,
          stroke = Some(stroke.copy(cap = CapRound))
        ).transform(_ => tx)

        re.render(ctx, shape1)

      }
    }
  }

}
