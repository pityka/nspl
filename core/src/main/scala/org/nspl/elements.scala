package org.nspl

case class ElemList[T <: Renderable[T]](
    members: Seq[T],
    tx: AffineTransform = AffineTransform.identity
) extends Renderable[ElemList[T]] {
  def transform(tx: (Bounds, AffineTransform) => AffineTransform) =
    this.copy(tx = tx(bounds, this.tx))

  def transform(tx: AffineTransform) = this.copy(tx = tx.applyBefore(this.tx))

  val bounds =
    if (members.size > 0)
      tx.transform(outline(members.iterator.map(_.bounds), anchor = None))
    else Bounds(0, 0, 0, 0)
}

object ElemList {
  implicit def compositeListRenderer[T <: Renderable[T], R <: RenderingContext[
    R
  ]](implicit
      r: Renderer[T, R]
  ) : Renderer[ElemList[T], R] =
    new Renderer[ElemList[T], R] {
      def render(ctx: R, elem: ElemList[T]): Unit = {
        ctx.withTransform(elem.tx) {
          elem.members.foreach(e => r.render(ctx, e))
        }
      }
    }
}

case class ElemList2[T1 <: Renderable[T1], T2 <: Renderable[T2]](
    members: Seq[Either[T1, T2]],
    tx: AffineTransform = AffineTransform.identity
) extends Renderable[ElemList2[T1, T2]] {
  def transform(tx: (Bounds, AffineTransform) => AffineTransform) =
    this.copy(tx = tx(bounds, this.tx))

  def transform(tx: AffineTransform) = this.copy(tx = tx.applyBefore(this.tx))

  val bounds =
    if (members.size > 0)
      tx.transform(outline(members.iterator.map(_.merge.bounds), anchor = None))
    else Bounds(0, 0, 0, 0)
}
object ElemList2 {

  implicit def compositeListRenderer2[T <: Renderable[T], T2 <: Renderable[
    T2
  ], R <: RenderingContext[R]](implicit
      r1: Renderer[T, R],
      r2: Renderer[T2, R]
  ): Renderer[ElemList2[T, T2], R] =
    new Renderer[ElemList2[T, T2], R] {
      def render(ctx: R, elem: ElemList2[T, T2]): Unit = {
        ctx.withTransform(elem.tx) {
          elem.members.foreach(e =>
            (e match {
              case scala.util.Left(e)  => r1.render(ctx, e)
              case scala.util.Right(e) => r2.render(ctx, e)
            })
          )
        }
      }
    }

}

case class ElemOption[A <: Renderable[A]](option: Option[A])
    extends Renderable[ElemOption[A]] {
  def transform(tx: (Bounds, AffineTransform) => AffineTransform) = ElemOption(
    option.map(_.transform(tx))
  )
  def transform(tx: AffineTransform) = ElemOption(option.map(_.transform(tx)))
  def bounds = option.fold(Bounds(0, 0, 0, 0))(_.bounds)
  def map[K <: Renderable[K]](f: A => K) = ElemOption(option.map(f))
}
object ElemOption {
  implicit def renderer[T1 <: Renderable[T1], R <: RenderingContext[R]](implicit
      r1: Renderer[T1, R]
  ): Renderer[ElemOption[T1], R] = new Renderer[ElemOption[T1], R] {
    def render(ctx: R, elem: ElemOption[T1]): Unit =
      elem.option.fold(())(r1.render(ctx, _))
  }
}
case class ElemEither[A <: Renderable[A], B <: Renderable[B]](
    either: Either[A, B],
    tx: AffineTransform = AffineTransform.identity
) extends Renderable[ElemEither[A, B]] {
  def transform(tx: (Bounds, AffineTransform) => AffineTransform) =
    this.copy(tx = tx(bounds, this.tx))
  def transform(tx: AffineTransform) = this.copy(tx = tx.applyBefore(this.tx))
  def bounds = tx.transform(either.fold(_.bounds, _.bounds))
}
object ElemEither {
  implicit def renderer[T1 <: Renderable[T1], T2 <: Renderable[
    T2
  ], R <: RenderingContext[R]](implicit
      r1: Renderer[T1, R],
      r2: Renderer[T2, R]
  ) : Renderer[ElemEither[T1, T2], R]= new Renderer[ElemEither[T1, T2], R] {
    def render(ctx: R, elem: ElemEither[T1, T2]): Unit =
      ctx.withTransform(elem.tx) {
        elem.either.fold(
          e => r1.render(ctx, e),
          e => r2.render(ctx, e)
        )
      }
  }
}

case class ShapeElem(
    shape: Shape,
    fill: Color = Color.black,
    strokeColor: Color = Color.black,
    stroke: Option[Stroke] = None,
    identifier: Identifier = EmptyIdentifier,
    tx: AffineTransform = AffineTransform.identity
) extends Renderable[ShapeElem] {

  def withIdentifier(id: Identifier) = copy(identifier = id)

  def transform(tx: (Bounds, AffineTransform) => AffineTransform) =
    this.copy(tx = tx(bounds, this.tx))

  def transform(tx: AffineTransform) = this.copy(tx = tx.applyBefore(this.tx))

  val bounds = {
    tx.transform(shape.bounds)
  }

}

case class TextBox(
    layout: TextLayout,
    color: Color,
    tx: AffineTransform
)(implicit fc: FontConfiguration)
    extends Renderable[TextBox] {

  val font = fc.font


  val bounds =
    if (layout.isEmpty) Bounds(0, 0, 0, 0)
    else tx.transform(layout.bounds)

  def transform(tx: (Bounds, AffineTransform) => AffineTransform) =
    this.copy(tx = tx(bounds, this.tx))
  def transform(tx: AffineTransform) = this.copy(tx = tx.applyBefore(this.tx))
}

object TextBox {
  def apply(
      text: String,
      width: Option[Double] = None,
      fontSize: RelFontSize = 1 fts,
      color: Color = Color.black,
      tx: AffineTransform = AffineTransform.identity
  )(implicit fc: FontConfiguration): TextBox =
    TextBox(TextLayout(width, text, fontSize),  color, tx)
}
