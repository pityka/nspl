package org.nspl

import data._

/** A DataRenderer can render a datum a a side effect
  *
  * DataRenderers describe the visual representations of single data rows.
  * DataRenderers operate in a side effect with the provided context specific
  * shape and textbox renderer.
  *
  * It is guaranteed that the render method is called in a loop on all rows of a
  * data source, after which the clear method is called exactly once.
  *
  * Data renderers interpret the data rows as it is applicable for their
  * function e.g. the point renderer takes 2 numbers for the x, y coordinates
  * and potentially numbers for the color value, and the top and bottom error
  * bars. In contrast the box and whiskes renderer takes 5 numbers for the
  * min/max/median/mean and the horizontal coordinate. If the data row is too
  * short for the given data renderer then an error is thrown.
  */
trait DataRenderer {

  /** Render one data row into `ctx`.
    *
    * @param descriptor
    *   identifier to attach to any rendered shapes for interactivity (e.g. a
    *   [[DataRowIdx]] supplied by the surrounding [[DataElem]]). Renderers
    *   that opt-in to per-row identification (most do via a `noIdentifier`
    *   flag defaulting to true) attach this to their emitted shapes; others
    *   simply ignore it.
    */
  def render[R <: RenderingContext[R]](
      data: Row,
      xAxis: Axis,
      yAxis: Axis,
      ctx: R,
      tx: AffineTransform,
      descriptor: Identifier
  )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit

  /** Compatibility overload — call with no descriptor. Forwards to the
    * descriptor-aware overload with [[EmptyIdentifier]]. Used by older
    * callers that haven't been threaded through the interactivity surface.
    */
  final def render[R <: RenderingContext[R]](
      data: Row,
      xAxis: Axis,
      yAxis: Axis,
      ctx: R,
      tx: AffineTransform
  )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit =
    render(data, xAxis, yAxis, ctx, tx, EmptyIdentifier)

  def asLegend: Option[LegendElem]
  @scala.annotation.nowarn
  def clear[R <: RenderingContext[R]](ctx: R)(implicit
      re: Renderer[ShapeElem, R],
      rt: Renderer[TextBox, R]
  ): Unit = ()
  def xMinMax(ds: DataSource): Option[MinMax]
  def yMinMax(ds: DataSource): Option[MinMax]
}

private[nspl] trait Renderers {

  /** A renderer which renders a data row as a point on a scatter plot */
  def point[T: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      colorCol: Int = 2,
      sizeCol: Int = 3,
      shapeCol: Int = 4,
      errorTopCol: Int = 5,
      errorBottomCol: Int = 6,
      errorLeftCol: Int = 9,
      errorRightCol: Int = 10,
      strokeColorCol: Int = 8,
      size: Double = 3d,
      color: Colormap = DiscreteColors(14),
      strokeColor: Option[Colormap] = None,
      strokeWidth: Option[Double] = None,
      shapes: Vector[Shape] = shapeList,
      pointSizeIsInDataSpaceUnits: Boolean = false,
      keepPointShapeAspectRatio: Boolean = false,
      valueText: Boolean = false,
      labelText: Boolean = false,
      labelFontSize: RelFontSize = 0.4 fts,
      labelColor: Color = Color.black,
      errorBarStroke: StrokeConf = StrokeConf(lineWidth),
      errorBarColor: Color = Color.black,
      transparent: Option[Double] = None,
      translate: (Double, Double) = (0d, 0d),
      xNoise: Double = 0d,
      yNoise: Double = 0d,
      label: Any => String = _.toString,
      noIdentifier: Boolean = true
  ) = new DataRenderer {

    def asLegend = Some(PointLegend(shapes.head, color(0)))

    def xMinMax(ds: DataSource) = ds.columnMinMax(xCol)
    def yMinMax(ds: DataSource) = ds.columnMinMax(yCol)

    val shapesAndTextLabels =
      scala.collection.mutable
        .ArrayBuffer[(ShapeElem, TextBox, AffineTransform)]()

    override def clear[R <: RenderingContext[R]](ctx: R)(implicit
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
            .transform(tx)

          rt.render(ctx, labelUpdated)
          connectionLine match {
            case None                                   =>
            case Some((p1, p2)) if p1.distance(p2) <= 0 =>
            case Some((p1, p2)) =>
              val lineElem = ShapeElem(
                Shape.line(p1, p2),
                strokeColor = labelColor,
                stroke = Some(Stroke(lineWidth.value * 0.3)),
                tx = tx
              )
              re.render(ctx, lineElem)
          }

      }
      shapesAndTextLabels.clear()
    }

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        descriptor: Identifier
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

          if (!skip) {
            val color1 = color(dataColorValue)
            val strokeColor1 = strokeColor.map { cm =>
              val v =
                if (data.dimension > strokeColorCol) data(strokeColorCol)
                else dataColorValue
              cm(v)
            }
            if (color1.a > 0 || strokeColor1.exists(_.a > 0)) {
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
              val factorY =
                unitWidthY * size1 * shapeAspectRatio / shapeBounds.h

              val noiseValueX =
                if (xNoise == 0d) 0d
                else
                  ((scala.util.Random
                    .nextDouble() - 0.5) / (xAxis.max - xAxis.min)) * xNoise
              val noiseValueY =
                if (yNoise == 0d) 0d
                else
                  ((scala.util.Random
                    .nextDouble() - 0.5) / (yAxis.max - yAxis.min)) * yNoise

              val vX = xAxis.worldToView(wX + translate._1 + noiseValueX)
              val vY = yAxis.worldToView(wY + translate._2 + noiseValueY)

              val pointStroke =
                strokeWidth.map(w => Stroke(w))
              val pointStrokeColor = strokeColor1.getOrElse(Color.black)

              val shape1PreTransform: ShapeElem =
                if (valueText || labelText)
                  ShapeElem(
                    shape,
                    fill = color1,
                    strokeColor = pointStrokeColor,
                    stroke = pointStroke,
                    tx = AffineTransform
                      .scaleThenTranslate(vX, vY, factorX, factorY)
                  )
                else null
              val shape1 =
                if (valueText || labelText) shape1PreTransform.transform(tx)
                else
                  ShapeElem(
                    shape,
                    fill = color1,
                    strokeColor = pointStrokeColor,
                    stroke = pointStroke,
                    tx = tx.scaleThenTranslate(vX, vY, factorX, factorY)
                  )
              if (data.dimension > errorTopCol) {
                val errorTop = data(errorTopCol)
                val shape1: ShapeElem = ShapeElem(
                  Shape.line(
                    Point(vX, vY),
                    Point(vX, yAxis.worldToView(errorTop))
                  ),
                  stroke = Some(errorBarStroke.value),
                  tx = tx,
                  strokeColor = errorBarColor
                )
                re.render(ctx, shape1)
              }
              if (data.dimension > errorBottomCol) {
                val errorBottom = data(errorBottomCol)
                val shape1: ShapeElem = ShapeElem(
                  Shape.line(
                    Point(vX, vY),
                    Point(vX, yAxis.worldToView(errorBottom))
                  ),
                  stroke = Some(errorBarStroke.value),
                  tx = tx,
                  strokeColor = errorBarColor
                )
                re.render(ctx, shape1)
              }
              if (data.dimension > errorLeftCol) {
                val errorLeft = data(errorLeftCol)
                val shape1: ShapeElem = ShapeElem(
                  Shape.line(
                    Point(vX, vY),
                    Point(xAxis.worldToView(errorLeft), vY)
                  ),
                  stroke = Some(errorBarStroke.value),
                  tx = tx,
                  strokeColor = errorBarColor
                )
                re.render(ctx, shape1)
              }
              if (data.dimension > errorRightCol) {
                val errorRight = data(errorRightCol)
                val shape1: ShapeElem = ShapeElem(
                  Shape.line(
                    Point(vX, vY),
                    Point(xAxis.worldToView(errorRight), vY)
                  ),
                  stroke = Some(errorBarStroke.value),
                  tx = tx,
                  strokeColor = errorBarColor
                )
                re.render(ctx, shape1)
              }
              val shape1WithId =
                if (noIdentifier) shape1
                else shape1.withIdentifier(descriptor)
              re.render(ctx, shape1WithId)

              if (valueText && data.dimension > colorCol) {
                val tbPreTransform = TextBox(
                  f"${data(colorCol)}%.2g",
                  color = labelColor,
                  fontSize = labelFontSize
                )
                  .transform((b, old) =>
                    old.translate(
                      vX,
                      vY + -1 * b.h - shape.bounds.h * factorY * 0.5
                    )
                  )

                shapesAndTextLabels += (
                  (
                    shape1PreTransform,
                    tbPreTransform,
                    tx
                  )
                )

              }

              if (labelText) {
                val tbPreTransform = TextBox(
                  label(data.label),
                  color = labelColor,
                  fontSize = labelFontSize
                )
                  .transform((b, old) =>
                    old.translate(
                      -0.2 * b.w + vX,
                      -1 * b.h - shape.bounds.h * factorY * 0.5 + vY
                    )
                  )

                shapesAndTextLabels += (
                  (
                    shape1PreTransform,
                    tbPreTransform,
                    tx
                  )
                )

              }
            }
          }

        }
      } else
        throw new RuntimeException(
          s"Record has no X or Y elements. size: ${data.dimension} vs idx $xCol $yCol"
        )
    }
  }

  /** A renderer which renders a data row as a sequence of joined line segments
    *
    * Each data row is connected with a line segment in the order they are
    * supplied in the data source
    */
  def line[F: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      colorCol: Int = 2,
      stroke: StrokeConf = StrokeConf(lineWidth),
      color: Colormap = Color.black
  ) = new DataRenderer {

    def xMinMax(ds: DataSource) = ds.columnMinMax(xCol)
    def yMinMax(ds: DataSource) = ds.columnMinMax(yCol)

    var currentPoint: Option[Point] = None
    def asLegend = Some(LineLegend(stroke.value, color(0)))

    override def clear[R <: RenderingContext[R]](ctx: R)(implicit
        re: Renderer[ShapeElem, R],
        rt: Renderer[TextBox, R]
    ): Unit = {
      currentPoint = None
    }

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        descriptor: Identifier
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
            stroke = Some(stroke.value.copy(cap = Cap.Round)),
            tx = tx
          )

          re.render(ctx, shape1)

          currentPoint = Some(p)

        }
      } else {
        // Reset so the next in-range point starts a new segment instead of
        // silently bridging the out-of-range gap.
        currentPoint = None
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

    override def clear[R <: RenderingContext[R]](ctx: R)(implicit
        re: Renderer[ShapeElem, R],
        rt: Renderer[TextBox, R]
    ): Unit = {
      currentPoint2 = None
      currentPoint1 = None
    }

    def xMinMax(ds: DataSource) = ds.columnMinMax(xCol)
    def yMinMax(ds: DataSource) = {
      val mm1 = ds.columnMinMax(yCol)
      val mm2 = yCol2.flatMap(ds.columnMinMax)
      // Area is bounded by both curves: take the union so neither one
      // clips out of view.
      mm1.map { a =>
        mm2 match {
          case Some(b) =>
            MinMaxImpl(math.min(a.min, b.min), math.max(a.max, b.max))
          case None =>
            // No second curve: area runs from 0 to yCol's max (legacy).
            MinMaxImpl(math.min(0d, a.min), a.max)
        }
      }
    }

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        descriptor: Identifier
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
            fill = color1,
            tx = tx
          )

          re.render(ctx, shape1)

          currentPoint1 = Some(p1)
          currentPoint2 = Some(p2)

        }
      }
    }
  }

  /** A renderer which renders a data row as a polynom
    *
    * It numerically evaluates the polynom sum(a_i x^i) and draws the resulting
    * curve
    */
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

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        descriptor: Identifier
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val r = renderer()

      0 to 1000 foreach { i =>
        val x = xAxis.min + i * (xAxis.max - xAxis.min) / 1000.0
        val y = evaluatePolynomial(data.allColumns.toArray, x)
        r.render(VectorRow(Vector(x, y), ""), xAxis, yAxis, ctx, tx, descriptor)
      }

    }
  }

  /** A renderer which renders a data row as a horizontal or vertical bar */
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

    def xMinMax(ds: DataSource): Option[MinMax] =
      if (horizontal) ds.columnMinMax(xCol)
      else {
        val bounds = ds.iterator.map { row =>
          val center = row(xCol)
          val actualWidth =
            if (row.allColumns.size > widthCol) row(widthCol) else width
          (center - actualWidth, center + actualWidth)
        }.toList
        if (bounds.isEmpty) None
        else Some(MinMaxImpl(bounds.minBy(_._1)._1, bounds.maxBy(_._2)._2))
      }
    def yMinMax(ds: DataSource): Option[MinMax] = if (!horizontal)
      ds.columnMinMax(yCol)
    else {
      val bounds = ds.iterator.map { row =>
        val center = row(yCol)
        val actualWidth =
          if (row.allColumns.size > widthCol) row(widthCol) else width
        (center - actualWidth, center + actualWidth)
      }.toList
      if (bounds.isEmpty) None
      else Some(MinMaxImpl(bounds.minBy(_._1)._1, bounds.maxBy(_._2)._2))
    }

    def asLegend = Some(PointLegend(shapeList(1), fill(0)))
    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        descriptor: Identifier
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

          // Compute the bar's left/right view coords directly — this works
          // for log axes too (where `worldToView(0)` would throw because
          // 0 is not in the log domain).
          val leftW = math.max(xAxis.min, wX - width1 * 0.5)
          val rightW = math.min(xAxis.max, wX + width1 * 0.5)
          val vL = xAxis.worldToView(leftW)
          val vR = xAxis.worldToView(rightW)
          val vWidth = math.abs(vR - vL)
          val vLeftEdge = math.min(vL, vR)

          val vY2 = yAxis.worldToView(wYBottom)
          val vY = yAxis.worldToView(wY)

          val vHeight = math.abs(vY2 - vY)

          val rectangle =
            if (vY2 > vY)
              Shape.rectangle(vLeftEdge, vY, vWidth, vHeight)
            else
              Shape.rectangle(vLeftEdge, vY2, vWidth, vHeight)

          val shape1 = ShapeElem(
            rectangle,
            fill = color1,
            stroke = if (stroke.width.value == 0d) None else Some(stroke.value),
            strokeColor = strokeColor,
            tx = tx
          )

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

          // Bug fix: previously used `width` instead of `width1`, ignoring
          // the per-row widthCol. Also log-axis-safe by going through bar
          // edges directly rather than via `worldToView(0)`.
          val topW = math.max(yAxis.min, wY - width1 * 0.5)
          val botW = math.min(yAxis.max, wY + width1 * 0.5)
          val vT = yAxis.worldToView(topW)
          val vB = yAxis.worldToView(botW)
          val vWidth = math.abs(vB - vT)
          val vTopEdge = math.min(vT, vB)

          val vX = xAxis.worldToView(wX)
          val vX2 = xAxis.worldToView(wXBottom)
          val vHeight = math.abs(vX2 - vX)

          val rectangle =
            if (wX > 0)
              Shape.rectangle(vX2, vTopEdge, vHeight, vWidth)
            else
              Shape.rectangle(vX, vTopEdge, vHeight, vWidth)

          val shape1 = ShapeElem(
            rectangle,
            fill = color1,
            stroke = if (stroke.width.value == 0d) None else Some(stroke.value),
            strokeColor = strokeColor,
            tx = tx
          )

          re.render(ctx, shape1)

        }

      }
    }
  }

  /** A renderer which renders a data row a single parameterized line (y=a+b*x)
    */
  def abline(
      a: Double,
      b: Double,
      renderer: DataRenderer
  ) =
    (dataSourceFromRows(Seq(a -> b)), List(renderer))

  /** A renderer which renders a data row as a box and whiskers plot
    *
    * The minimum, maximum, median and mean are rendered.
    */
  def boxwhisker[F: FC](
      xCol: Int = 0,
      medianCol: Int = 1,
      q1Col: Int = 2,
      q3Col: Int = 3,
      minCol: Int = 4,
      maxCol: Int = 5,
      widthCol: Int = 6,
      fillCol: Int = 7,
      width: Double = 1,
      stroke: StrokeConf = StrokeConf(lineWidth),
      strokeColor: Color = Color.black,
      fill: Colormap = Color.white
  ) = new DataRenderer {
    def asLegend = Some(PointLegend(shapeList(1), fill(0)))

    def xMinMax(ds: DataSource): Option[MinMax] = {
      val bounds = ds.iterator.map { row =>
        val mid = row(xCol)
        val w = if (row.allColumns.size > widthCol) row(widthCol) else width
        (mid - w, mid + w)
      }.toList
      if (bounds.isEmpty) None
      else Some(MinMaxImpl(bounds.minBy(_._1)._1, bounds.maxBy(_._2)._2))
    }
    def yMinMax(ds: DataSource): Option[MinMax] = {
      val min = ds.columnMinMax(minCol).map(_.min)
      val max = ds.columnMinMax(maxCol).map(_.max)
      for {
        min <- min
        max <- max
      } yield MinMaxImpl(min, max)
    }

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        descriptor: Identifier
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX1 = data(xCol)
      val q2 = data(medianCol)
      val q1 = data(q1Col)
      val q3 = data(q3Col)
      val min = data(minCol)
      val max = data(maxCol)
      val color1 =
        if (data.dimension > fillCol) fill(data(fillCol)) else fill(wX1)
      val width1 = if (data.dimension > widthCol) data(widthCol) else width
      val wX = wX1

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
          strokeColor = strokeColor,
          tx = tx
        )

        re.render(ctx, shape1)

        val shape2 = ShapeElem(
          Shape
            .line(Point(vX - vWidth * 0.5, vQ2), Point(vX + vWidth * 0.5, vQ2)),
          fill = color1,
          stroke = Some(stroke.value),
          strokeColor = strokeColor,
          tx = tx
        )

        re.render(ctx, shape2)

        val shape3 = ShapeElem(
          Shape.line(Point(vX, vQ1), Point(vX, vMin)),
          fill = color1,
          stroke = Some(stroke.value),
          strokeColor = strokeColor,
          tx = tx
        )

        re.render(ctx, shape3)

        val shape4 = ShapeElem(
          Shape.line(Point(vX, vQ3), Point(vX, vMax)),
          fill = color1,
          stroke = Some(stroke.value),
          strokeColor = strokeColor,
          tx = tx
        )

        re.render(ctx, shape4)

      }
    }
  }

  /** A renderer which renders a data row as series of potentially disconnected
    * line segments
    *
    * Each data row must provide both endpoints of the line segment
    */
  def lineSegment[F: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      x2Col: Int = 2,
      y2Col: Int = 3,
      colorCol: Int = 4,
      stroke: StrokeConf = StrokeConf(lineWidth, Cap.Round),
      color: Colormap = HeatMapColors(0, 1),
      labelText: Boolean = false,
      labelColor: Color = Color.black,
      labelFontSize: RelFontSize = 0.4 fts,
      labelDistance: RelFontSize = 0.4 fts,
      label: Any => String = _.toString
  ) = new DataRenderer {

    def xMinMax(ds: DataSource): Option[MinMax] = {
      val m1 = ds.columnMinMax(xCol)
      val m2 = ds.columnMinMax(x2Col)
      for { m1 <- m1; m2 <- m2 } yield MinMaxImpl(
        math.min(m1.min, m2.min),
        math.max(m1.max, m2.max)
      )
    }

    def yMinMax(ds: DataSource): Option[MinMax] = {
      val m1 = ds.columnMinMax(yCol)
      val m2 = ds.columnMinMax(y2Col)
      for { m1 <- m1; m2 <- m2 } yield MinMaxImpl(
        math.min(m1.min, m2.min),
        math.max(m1.max, m2.max)
      )
    }

    def asLegend = Some(LineLegend(stroke.value, color(0)))

    def render[R <: RenderingContext[R]](
        data: Row,
        xAxis: Axis,
        yAxis: Axis,
        ctx: R,
        tx: AffineTransform,
        descriptor: Identifier
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
          stroke = Some(stroke.value),
          tx = tx
        )

        re.render(ctx, shape1)

        if (labelText) {
          val cX = (vX + vX2) * 0.5
          val cY = (vY + vY2) * 0.5
          val (nX, nY) = {
            val ax = vX2 - vX
            val ay = vY2 - vY
            val l = math.sqrt(ax * ax + ay * ay)
            (-1 * ay / l, ax / l)
          }
          val lX = cX + labelDistance.value * nX
          val lY = cY + labelDistance.value * nY

          val textBox = TextBox(
            label(data.label),
            color = labelColor,
            fontSize = labelFontSize
          ).translate(lX, lY)
            .transform((b, old) =>
              old.andThen(tx.translate(-1 * b.w * 0.5, 0d))
            )
          rt.render(ctx, textBox)
        }

      }
    }
  }

}
