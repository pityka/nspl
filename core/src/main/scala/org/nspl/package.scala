package org

package object nspl
    extends Tuples1
    with Tuples2
    with Colors
    with Shapes
    with Renderers
    with data.DataAdaptors
    with Plots
    with SimplePlots
    with ImplicitConversions {

  type AxisElem = Elems3[ShapeElem, ElemList[Elems2[ShapeElem, TextBox]], ElemList[ShapeElem]]

  implicit val fontSize = BaseFontSize(10d)

  implicit def rel2ft(v: RelFontSize)(implicit s: BaseFontSize): Double = v.v * s.v //* 1.5

  implicit class ConvD(v: Double) {
    def fts = RelFontSize(v)
  }
  implicit class ConvI(v: Int) {
    def fts = RelFontSize(v.toDouble)
  }
  implicit class ConvRFS(v: RelFontSize) {
    def value = rel2ft(v)
  }

  /* Calculates the total bounds of the members. */
  def outline(members: Seq[Bounds]) = {
    val x = members.map(_.x).min
    val y = members.map(_.y).min
    val maxX = members.map(_.maxX).max
    val maxY = members.map(_.maxY).max
    val w = maxX - x
    val h = maxY - y
    Bounds(x, y, w, h)
  }

  def transform[T <: Renderable[T]](member: T, tx: Bounds => AffineTransform): T =
    member.transform(tx)

  def translate[T <: Renderable[T]](member: T, x: Double, y: Double): T =
    member.translate(x, y)

  def rotate[T <: Renderable[T]](
    member: T, rad: Double, x: Double, y: Double
  ): T = member.rotate(rad, x, y)

  def rotate[T <: Renderable[T]](member: T, rad: Double) =
    member.rotate(rad)

  def reflectOrigin[T <: Renderable[T]](member: T) =
    member.reflectOrigin

  def reflectX[T <: Renderable[T]](member: T) =
    member.reflectX

  def rotateCenter[T <: Renderable[T]](member: T, rad: Double) =
    member.rotateCenter(rad)

  def reflectY[T <: Renderable[T]](member: T) =
    member.reflectY

  def scale[T <: Renderable[T]](member: T, x: Double, y: Double) =
    member.scale(x, y)

  def fitToBounds[T <: Renderable[T]](member: T, bounds: Bounds) =
    {
      val current = member.bounds
      scale(
        translate(member, bounds.x - current.x, bounds.y - current.y),
        if (current.w != 0d) bounds.w / current.w else 1d,
        if (current.h != 0d) bounds.h / current.h else 1d
      )
    }

  def sequence[T <: Renderable[T]](members: List[T], layout: Layout = FreeLayout) =
    {
      val orig = members.map(_.bounds)
      val n = layout(orig)
      val transformed = n zip members map (x => fitToBounds(x._2, x._1))
      ElemList(transformed.toList)
    }

  implicit def compositeListRenderer[T <: Renderable[T], R <: RenderingContext](implicit r: Renderer[T, R]) =
    new Renderer[ElemList[T], R] {
      def render(ctx: R, elem: ElemList[T]): Unit = {
        elem.members.foreach(e => r.render(ctx, e))
      }
    }

  implicit def dataElemRenderer[RC <: RenderingContext](implicit re: Renderer[ShapeElem, RC], rt: Renderer[TextBox, RC]) = new Renderer[DataElem, RC] {
    def render(r: RC, e: DataElem): Unit = {
      e.data.iterator.foreach { row =>
        e.renderers.foreach { dr =>
          dr.render(row, e.xAxis, e.yAxis, r, e.tx)
        }
      }
      e.renderers.foreach(_.clear)
    }
  }

  /* Normalized scientific notation. */
  def scientific(x: Double) = x / math.pow(10d, math.log10(x).round) -> math.log10(x).round

}
