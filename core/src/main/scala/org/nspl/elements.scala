package org.nspl

case class ElemList[T <: Renderable[T]](members: Seq[T])
    extends Renderable[ElemList[T]] {
  def transform(tx: Bounds => AffineTransform) =
    ElemList(members.map(_.transform(tx)))
  val bounds =
    if (members.size > 0) outline(members.iterator.map(_.bounds), anchor = None)
    else Bounds(0, 0, 0, 0)
}

object ElemList {
  implicit def compositeListRenderer[T <: Renderable[T], R <: RenderingContext](
      implicit r: Renderer[T, R]
  ) =
    new Renderer[ElemList[T], R] {
      def render(ctx: R, elem: ElemList[T]): Unit = {
        elem.members.foreach(e => r.render(ctx, e))
      }
    }
}

case class ElemList2[T1 <: Renderable[T1], T2 <: Renderable[T2]](
    members: Seq[Either[T1, T2]]
) extends Renderable[ElemList2[T1, T2]] {
  def transform(tx: Bounds => AffineTransform) =
    ElemList2(members.map(_ match {
      case scala.util.Left(x)  => scala.util.Left(x.transform(tx))
      case scala.util.Right(x) => scala.util.Right(x.transform(tx))
    }))
  val bounds =
    if (members.size > 0)
      outline(members.iterator.map(_.merge.bounds), anchor = None)
    else Bounds(0, 0, 0, 0)
}
object ElemList2 {

  implicit def compositeListRenderer2[T <: Renderable[T], T2 <: Renderable[
    T2
  ], R <: RenderingContext](implicit
      r1: Renderer[T, R],
      r2: Renderer[T2, R]
  ) =
    new Renderer[ElemList2[T, T2], R] {
      def render(ctx: R, elem: ElemList2[T, T2]): Unit = {
        elem.members.foreach(e =>
          (e match {
            case scala.util.Left(e)  => r1.render(ctx, e)
            case scala.util.Right(e) => r2.render(ctx, e)
          })
        )
      }
    }

}

case class ShapeElem(
    shape: Shape,
    fill: Color = Color.black,
    strokeColor: Color = Color.black,
    stroke: Option[Stroke] = None,
    scaleStroke: Boolean = true,
    identifier: Identifier = EmptyIdentifier
) extends Renderable[ShapeElem] {

  def withIdentifier(id: Identifier) = copy(identifier = id)

  def transform(tx: Bounds => AffineTransform) =
    this.copy(
      shape = shape.transform(tx),
      stroke = stroke.map { stroke =>
        if (scaleStroke) {
          val oldWidth = stroke.width
          val tx1 = tx(bounds)
          val fx = math.min(math.min(tx1.m0, tx1.m4), 1d)
          Stroke(oldWidth * fx)
        } else stroke
      }
    )

  val bounds = {
    shape.bounds
  }

}

case class TextBox(
    layout: TextLayout,
    loc: Point,
    color: Color,
    tx: AffineTransform
)(implicit fc: FontConfiguration)
    extends Renderable[TextBox] {

  val font = fc.font

  val txLoc = tx.concat(AffineTransform.translate(loc.x, loc.y))

  val bounds =
    if (layout.isEmpty) Bounds(0, 0, 0, 0)
    else txLoc.transform(layout.bounds)

  def transform(tx: Bounds => AffineTransform) =
    this.copy(tx = tx(bounds).concat(this.tx))
}

object TextBox {
  def apply(
      text: String,
      loc: Point = Point(0d, 0d),
      width: Option[Double] = None,
      fontSize: RelFontSize = 1 fts,
      color: Color = Color.black,
      tx: AffineTransform = AffineTransform.identity
  )(implicit fc: FontConfiguration): TextBox =
    TextBox(TextLayout(width, text, fontSize), loc, color, tx)
}
