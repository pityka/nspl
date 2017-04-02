package org.nspl

case class ElemList[T <: Renderable[T]](members: Seq[T])
    extends Renderable[ElemList[T]] {
  def transform(tx: Bounds => AffineTransform) = ElemList(members.map(_.transform(tx)))
  def bounds = if (members.size > 0) outline(members.map(_.bounds)) else Bounds(0, 0, 0, 0)
}

case class ElemList2[T1 <: Renderable[T1], T2 <: Renderable[T2]](members: Seq[Either[T1, T2]])
    extends Renderable[ElemList2[T1, T2]] {
  def transform(tx: Bounds => AffineTransform) = ElemList2(members.map(_ match {
    case scala.util.Left(x) => scala.util.Left(x.transform(tx))
    case scala.util.Right(x) => scala.util.Right(x.transform(tx))
  }))
  def bounds = if (members.size > 0) outline(members.map(_.merge.bounds)) else Bounds(0, 0, 0, 0)
}

case class ShapeElem(
    shape: Shape,
    fill: Color = Color.black,
    strokeColor: Color = Color.black,
    stroke: Option[Stroke] = None
) extends Renderable[ShapeElem] {

  def transform(tx: Bounds => AffineTransform) =
    this.copy(shape = shape.transform(tx))

  def bounds = {
    val b = shape.bounds
    stroke.map(stroke =>
      Bounds(b.x - 0.5 * stroke.width, b.y - 0.5 * stroke.width, b.w + 0.5 * stroke.width, b.h + 0.5 * stroke.width)) getOrElse (b)
  }

}

case class TextBox(
    text: String,
    loc: Point = Point(0d, 0d),
    width: Option[Double] = None,
    fontSize: RelFontSize = 1 fts,
    color: Color = Color.black,
    tx: AffineTransform = AffineTransform.identity
)(implicit fc: FontConfiguration) extends Renderable[TextBox] {

  val layout: TextLayout = TextLayout(width, text, fontSize)

  val font = fc.font

  val txLoc = tx.concat(AffineTransform.translate(loc.x, loc.y))

  def bounds =
    if (text.isEmpty) Bounds(0, 0, 0, 0)
    else txLoc.transform(layout.bounds)

  def transform(tx: Bounds => AffineTransform) = this.copy(tx = tx(bounds).concat(this.tx))
}
