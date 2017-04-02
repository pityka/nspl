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

  implicit def defaultFont = Monospace

  implicit def baseFont(implicit fc: FontConfiguration): BaseFontSize = BaseFontSize(fc.font.size)

  implicit def rel2ft(v: RelFontSize)(implicit s: BaseFontSize): Double = v.v * s.v

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

  def fitToWidth[T <: Renderable[T]](elem: T, width: Double) = {
    val aspect = elem.bounds.h / elem.bounds.w
    val height = (width * aspect).toInt
    val bounds = Bounds(0, 0, width, height)
    fitToBounds(elem, bounds)
  }

  def sequence[T <: Renderable[T]](members: Seq[T], layout: Layout = FreeLayout) =
    {
      val orig = members.map(_.bounds)
      val n = layout(orig)
      val transformed = n zip members map (x => fitToBounds(x._2, x._1))
      ElemList(transformed.toList)
    }

  def sequence2[T1 <: Renderable[T1], T2 <: Renderable[T2]](members: Seq[Either[T1, T2]], layout: Layout = FreeLayout): ElemList2[T1, T2] =
    {
      val orig = members.map(_ match {
        case scala.util.Left(x) => x.bounds
        case scala.util.Right(x) => x.bounds
      })
      val n = layout(orig)
      val transformed = n zip members map (x => x._2 match {
        case scala.util.Left(y) => scala.util.Left(fitToBounds(y, x._1))
        case scala.util.Right(y) => scala.util.Right(fitToBounds(y, x._1))
      })
      ElemList2(transformed)
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
