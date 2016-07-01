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
  def bounds = {
    if (text.isEmpty) Bounds(0, 0, 0, 0)
    else {
      val charWidth = fontSize * 0.6
      width.map { width =>
        val maxCharInLine = math.min((width / charWidth).toInt, text.size)
        val lines = text.size / maxCharInLine
        val finalLine = if (lines * maxCharInLine >= text.size) 0 else 1
        val h = (lines + finalLine) * fontSize * 1.15
        tx.transform(Bounds(loc.x, loc.y, maxCharInLine * charWidth, h))
      }.getOrElse(tx.transform(Bounds(loc.x, loc.y, text.size * charWidth, fontSize * 1.15)))
    }
  }
  def transform(tx: Bounds => AffineTransform) = this.copy(tx = tx(bounds).concat(this.tx))
}
