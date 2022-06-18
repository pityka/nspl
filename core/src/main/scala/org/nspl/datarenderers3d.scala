package org.nspl

import data._
import scala.util.Try

trait DataRenderer3D {
  def render[R <: RenderingContext[R]](
      data: Row,
      ctx: R,
      viewProjectionMatrix: Math3D.Mat4,
      tx: AffineTransform
  )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit
  def clear[R <: RenderingContext[R]](ctx: R)(implicit
      re: Renderer[ShapeElem, R],
      rt: Renderer[TextBox, R]
  ): Unit = ()
  def asLegend: Option[LegendElem] = None
}

trait Renderers3D {
  def lineSegment3D[F: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      zCol: Int = 2,
      x2Col: Int = 3,
      y2Col: Int = 4,
      z2Col: Int = 5,
      colorCol: Int = 6,
      stroke: StrokeConf = StrokeConf(lineWidth * 0.01, Cap.Round),
      color: Colormap = HeatMapColors(0, 1)
  ) = new DataRenderer3D {

    def render[R <: RenderingContext[R]](
        data: Row,
        ctx: R,
        viewProjectionMatrix: Math3D.Mat4,
        tx: AffineTransform
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol).toFloat
      val wY = data(yCol).toFloat
      val wZ = data(zCol).toFloat
      val wX2 = data(x2Col).toFloat
      val wY2 = data(y2Col).toFloat
      val wZ2 = data(z2Col).toFloat

      val matrix1 =
        Math3D.translate(viewProjectionMatrix, wX, wY, wZ)
      val matrix2 =
        Math3D.translate(viewProjectionMatrix, wX2, wY2, wZ2)

      val modelCoord = Math3D.Vec4(0f, 0f, 0f, 1f)
      val clip1 =
        Math3D.perspectiveDivide(Math3D.vectorMultiply(modelCoord, matrix1))
      val clip2 =
        Math3D.perspectiveDivide(Math3D.vectorMultiply(modelCoord, matrix2))

      val inbounds = clip1(0) > -1d &&
        clip1(0) < 1d &&
        clip1(1) < 1d &&
        clip1(1) > -1d &&
        clip2(0) > -1d &&
        clip2(0) < 1d &&
        clip2(1) < 1d &&
        clip2(1) > -1d

      if (inbounds) {
        val color1 =
          if (data.dimension > colorCol) color(data(colorCol)) else color(0d)
        val shape1 = ShapeElem(
          Shape.line(clip1.toPoint, clip2.toPoint),
          strokeColor = color1,
          stroke = Some(stroke.value)
        ).transform(tx)

        re.render(ctx, shape1)
      }

    }
  }
  def point3D[F: FC](
      xCol: Int = 0,
      yCol: Int = 1,
      zCol: Int = 2,
      colorCol: Int = 3,
      sizeCol: Int = 4,
      size: Double = 0.5d,
      stroke: StrokeConf = StrokeConf(lineWidth, Cap.Round),
      color: Colormap = HeatMapColors(0, 1),
      shape: Shape = shapeList(0),
      keepPointShapeAspectRatio: Boolean = true
  ) = new DataRenderer3D {

    def render[R <: RenderingContext[R]](
        data: Row,
        ctx: R,
        viewProjectionMatrix: Math3D.Mat4,
        tx: AffineTransform
    )(implicit re: Renderer[ShapeElem, R], rt: Renderer[TextBox, R]): Unit = {

      val wX = data(xCol).toFloat
      val wY = data(yCol).toFloat
      val wZ = data(zCol).toFloat

      val matrix1 =
        Math3D.translate(viewProjectionMatrix, wX, wY, wZ)

      val modelCoord = Math3D.Vec4(0f, 0f, 0f, 1f)
      val clip1 =
        Math3D.perspectiveDivide(Math3D.vectorMultiply(modelCoord, matrix1))

      val inbounds = clip1(0) > -1d &&
        clip1(0) < 1d &&
        clip1(1) < 1d &&
        clip1(1) > -1d

      if (inbounds) {
        val dataColorValue =
          if (data.dimension > colorCol) data(colorCol) else 0d

        val color1 = color(dataColorValue)
        val size1 = if (data.dimension > sizeCol) data(sizeCol) else size
        val shapeBounds = shape.bounds
        val shapeAspectRatio =
          if (keepPointShapeAspectRatio) shapeBounds.h / shapeBounds.w
          else 1d

        val clip2 = {
          Math3D.perspectiveDivide(
            Math3D
              .vectorMultiply(
                Math3D.Vec4(shapeBounds.w.toFloat, 0f, 0f, 1f),
                matrix1
              )
          )
        }
        val perspectivFactorX = size1 * math
          .abs(clip1(0) - clip2(0)) / shapeBounds.w
        val perspectivFactorY = size1 * shapeAspectRatio * math
          .abs(clip1(0) - clip2(0)) / shapeBounds.h

        val vX = clip1(0)
        val vY = clip1(1)
        val shape1PreTransform: ShapeElem = ShapeElem(
          shape.transform((_,old) =>
            old.scaleThenTranslate(tx=clip1(0),ty=vY,sx=perspectivFactorX,sy=perspectivFactorY)
          ),
          fill = color1
        )

        val shape1 = shape1PreTransform.transform(tx)

        re.render(ctx, shape1)
      }

    }
  }
}
