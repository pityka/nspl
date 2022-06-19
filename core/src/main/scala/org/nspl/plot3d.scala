package org.nspl

import data._
import Align._

case class DataElem3D(
    data: DataSource,
    renderers: Seq[DataRenderer3D],
    matrix: Math3D.Mat4,
    originalBounds: Bounds = Bounds(-1, -1, 2, 2),
    tx: AffineTransform = AffineTransform.identity
) extends Renderable[DataElem3D] {
  def transform(tx: AffineTransform) = this.copy(tx = tx.applyBefore(this.tx))
  def transform(tx: (Bounds, AffineTransform) => AffineTransform) = {
    val ntx = tx(bounds, this.tx)
    this.copy(tx = ntx)
  }
  def bounds = tx.transform(originalBounds)
}

object DataElem3D {
  implicit def dataElemRenderer[RC <: RenderingContext[RC]](implicit
      re: Renderer[ShapeElem, RC],
      rt: Renderer[TextBox, RC]
  ): Renderer[DataElem3D, RC] = new Renderer[DataElem3D, RC] {
    def render(r: RC, e: DataElem3D): Unit = {
      e.data.iterator.foreach { row =>
        e.renderers.foreach { dr =>
          dr.render(row, r, e.matrix, e.tx)
        }
      }
      e.renderers.foreach(_.clear(r))
    }
  }
}

private[nspl] trait Plots3D {
  import Math3D._

  private type T =
    org.nspl.Elems3[org.nspl.ShapeElem, org.nspl.Elems3[
      org.nspl.ShapeElem,
      org.nspl.Elems2[org.nspl.ShapeElem, org.nspl.ElemList[
        org.nspl.DataElem3D
      ]],
      org.nspl.ShapeElem
    ], org.nspl.ShapeElem]

  case class XYZPlotArea(elem: T, cameraPosition: Vec3, cameraTarget: Vec3)
      extends Renderable[XYZPlotArea] {
    def transform(v: AffineTransform) = copy(elem = elem.transform(v))
    def transform(v: (Bounds, AffineTransform) => AffineTransform) =
      this.copy(elem = elem.transform(v))
    def bounds: Bounds = elem.bounds
  }

  object XYZPlotArea {
    implicit def renderer[RC <: RenderingContext[RC]](implicit
        re: Renderer[ShapeElem, RC],
        rt: Renderer[TextBox, RC]
    ): Renderer[XYZPlotArea, RC] = new Renderer[XYZPlotArea, RC] {
      def render(r: RC, e: XYZPlotArea): Unit =
        implicitly[Renderer[T, RC]].render(r, e.elem)
    }
  }

  def xyzplotareaBuild[F: FC](
      data: Seq[(DataSource, List[DataRenderer3D])],
      aspect: Double,
      zNear: Double,
      zFar: Double,
      fieldOfViewAngles: Double,
      cameraPosition: Vec3,
      cameraTarget: Vec3,
      // main: String = "",
      // mainFontSize: RelFontSize = 1 fts,
      topPadding: RelFontSize,
      bottomPadding: RelFontSize,
      leftPadding: RelFontSize,
      rightPadding: RelFontSize,
      xWidth: RelFontSize,
      yHeight: RelFontSize
      // mainLabDistance: RelFontSize = 0.75 fts
  ) = {
    val id = new PlotId
    Build(
      xyzplotarea(
        id,
        data,
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
    ) {
      case (Some(old), Scroll(v1, _, plotAreaId)) if plotAreaId.id == id =>
        val speed = math.abs(zFar - zNear) / 500
        val v = (if (v1 > 0) speed else if (v1 < 0) -speed else 0.0)

        val cameraDirection =
          normalize(subtractVectors(old.cameraTarget, old.cameraPosition))
        val newCameraPosition =
          old.cameraPosition + cameraDirection * (-v.toFloat)

        xyzplotarea(
          id,
          data,
          aspect,
          zNear,
          zFar,
          fieldOfViewAngles,
          newCameraPosition,
          old.cameraTarget,
          topPadding,
          bottomPadding,
          leftPadding,
          rightPadding,
          xWidth,
          yHeight
        )

      case (Some(old), Drag(dragStart, dragTo, plotAreaId))
          if plotAreaId.id == id =>
        val dX = (dragTo.x - dragStart.x)
        val dY = (dragTo.y - dragStart.y)

        val upperHalf =
          dragStart.y > plotAreaId.bounds.get.y + plotAreaId.bounds.get.h * 0.5

        val newCameraTarget =
          if (upperHalf)
            old.cameraTarget
              .withX(old.cameraTarget(0) - dX.toFloat * 0.7f)
              .withY(old.cameraTarget(1) + dY.toFloat * 0.7f)
          else old.cameraTarget

        val newCameraPosition =
          if (!upperHalf)
            old.cameraPosition
              .withX(old.cameraPosition(0) + dX.toFloat)
              .withY(old.cameraPosition(1) + dY.toFloat)
          else old.cameraPosition

        xyzplotarea(
          id,
          data,
          aspect,
          zNear,
          zFar,
          fieldOfViewAngles,
          newCameraPosition,
          newCameraTarget,
          topPadding,
          bottomPadding,
          leftPadding,
          rightPadding,
          xWidth,
          yHeight
        )
    }
  }

  def xyzplotarea[F: FC](
      id: PlotId,
      data: Seq[(DataSource, List[DataRenderer3D])],
      aspect: Double,
      zNear: Double,
      zFar: Double,
      fieldOfViewAngles: Double,
      cameraPosition: Vec3,
      cameraTarget: Vec3,
      // main: String = "",
      // mainFontSize: RelFontSize = 1 fts,
      topPadding: RelFontSize,
      bottomPadding: RelFontSize,
      leftPadding: RelFontSize,
      rightPadding: RelFontSize,
      xWidth: RelFontSize,
      yHeight: RelFontSize
      // mainLabDistance: RelFontSize = 0.75 fts
  ) = {

    val fieldOfViewRadians = degToRad(fieldOfViewAngles)
    val projectionMatrix = perspective(
      fieldOfViewRadians,
      aspect.toFloat,
      zNear.toFloat,
      zFar.toFloat
    )

    val up = Vec3(0, 1, 0)

    val cameraMatrix = lookAt(cameraPosition, cameraTarget, up)
    val viewMatrix = inverse(cameraMatrix)
    val viewProjectionMatrix = multiply(projectionMatrix, viewMatrix)

    val clipFrame = ShapeElem(
      Shape.rectangle(-1, -1, 2, 2),
      stroke = None,
      fill = Color.transparent,
      strokeColor = Color.black
    ).withIdentifier(PlotAreaIdentifier(id, None))

    val dataelem = sequence(data.toList.map { case (ds, drs) =>
      DataElem3D(ds, drs, viewProjectionMatrix)
    })

    val padTop = ShapeElem(
      shape = Shape.circle(topPadding.value),
      fill = Color.transparent,
      strokeColor = Color.transparent,
      stroke = None
    )

    val padBottom = ShapeElem(
      shape = Shape.circle(bottomPadding.value),
      fill = Color.transparent,
      strokeColor = Color.transparent,
      stroke = None
    )

    val padLeft = ShapeElem(
      shape = Shape.circle(leftPadding.value),
      fill = Color.transparent,
      strokeColor = Color.transparent,
      stroke = None
    )

    val padRight = ShapeElem(
      shape = Shape.circle(rightPadding.value),
      fill = Color.transparent,
      strokeColor = Color.transparent,
      stroke = None
    )

    val plotWithFrame = group(clipFrame, dataelem, FreeLayout)

    val elem = group(
      padTop,
      group(
        padLeft,
        plotWithFrame,
        padRight,
        HorizontalStack(Center, 0d fts)
      ),
      padBottom,
      VerticalStack(Center, 0d fts)
    )

    fitToBounds(
      XYZPlotArea(
        elem,
        cameraPosition,
        cameraTarget
      ),
      Bounds(0, 0, xWidth.value, yHeight.value)
    )

  }

}
