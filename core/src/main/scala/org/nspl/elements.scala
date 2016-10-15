package org.nspl

case class ElemList[T <: Renderable[T]](members: Seq[T])
    extends Renderable[ElemList[T]] {
  def transform(tx: Bounds => AffineTransform) = ElemList(members.map(_.transform(tx)))
  def bounds = if (members.size > 0) outline(members.map(_.bounds)) else Bounds(0, 0, 0, 0)
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
) extends Renderable[TextBox] {

  val charWidth = fontSize * 0.67

  val maxCharInLine = width.map(width => math.min((width / charWidth).toInt, text.size)).getOrElse(text.size)

  val lineWidth = width.map { width =>
    maxCharInLine * charWidth
  } getOrElse text.size * charWidth

  val lineNumber = width.map { width =>
    val lines = if (text.isEmpty) 1 else text.size / maxCharInLine
    val finalLine = if (lines * maxCharInLine >= text.size) 0 else 1
    (lines + finalLine)
  } getOrElse 1

  val lineHeight = fontSize * 1.2
  val lineAscentHeight = 1d

  def bounds =
    if (text.isEmpty) Bounds(0, 0, 0, 0)
    else tx.transform(Bounds(loc.x, loc.y, lineWidth, lineNumber * lineHeight + lineAscentHeight * math.max(0, lineNumber - 1)))

  def transform(tx: Bounds => AffineTransform) = this.copy(tx = tx(bounds).concat(this.tx))
}
