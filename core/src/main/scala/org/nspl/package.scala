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
    with ImplicitConversions
    with Plots3D
    with Renderers3D
    with Events {

  type Build[A] = ((Option[A], Event)) => A // Option[A]

  implicit class defaultBuild[T](b: Build[T]) {
    def build: T = b(None -> BuildEvent)
  }

  type AxisElem =
    Elems3[ShapeElem, ElemList[Elems2[ShapeElem, TextBox]], ElemList[ShapeElem]]

  type FC[_] = FontConfiguration

  implicit class ftsSyntaxDouble(v: Double) {
    def fts = new RelFontSize(v)
  }
  implicit class ftsSyntaxInt(v: Int) {
    def fts = new RelFontSize(v.toDouble)
  }

  def font(name: String)(implicit gm: Font.GlyphMeasurer[Font.Named]) =
    new Font.MeasuredFontConfiguration(Font.Named(name, 10), gm)

  /* Calculates the total bounds of the members. */
  private[nspl] def outline(members1: Iterator[Bounds], anchor: Option[Point]) = {
    var empty = true
    var minX = Double.MaxValue
    var minY = Double.MaxValue
    var maxX = Double.MinValue
    var maxY = Double.MinValue

    members1.foreach { t =>
      if (t.w > 0 || t.h > 0) {
        empty = false
        if (t.x < minX) {
          minX = t.x
        }
        if (t.maxX > maxX) {
          maxX = t.maxX
        }
        if (t.y < minY) {
          minY = t.y
        }
        if (t.maxY > maxY) {
          maxY = t.maxY
        }
      }
    }

    if (empty) Bounds(0, 0, 0, 0, anchor)
    else {

      val w = maxX - minX
      val h = maxY - minY
      Bounds(minX, minY, w, h, anchor)
    }
  }

  def fitToBounds[T <: Renderable[T]](member: T, bounds: Bounds) = {
    val current = member.bounds
    member.transform(
      AffineTransform.translateThenScale(
        tx = bounds.x - current.x,
        ty = bounds.y - current.y,
        sx = if (current.w != 0d) bounds.w / current.w else 1d,
        sy = if (current.h != 0d) bounds.h / current.h else 1d
      )
    )
  }

  implicit def renderable2build[T <: Renderable[T]](elem: T): Build[T] =
    Build.const(elem)

  def fitToWidth[T <: Renderable[T]](elem: T, width: Double) = {
    val aspect = elem.bounds.h / elem.bounds.w
    val height = (width * aspect).toInt
    val bounds = Bounds(0, 0, width, height)
    fitToBounds(elem, bounds)
  }
  def fitToHeight[T <: Renderable[T]](elem: T, height: Double) = {
    if (elem.bounds.h != 0) {
      val aspect = elem.bounds.w / elem.bounds.h
      val width = (height * aspect).toInt
      val bounds = Bounds(0, 0, width, height)
      fitToBounds(elem, bounds)
    } else elem
  }

  def sequence[T <: Renderable[T], F: FC](
      members: Seq[T],
      layout: Layout
  ): ElemList[T] = {
    val orig = members.map(_.bounds)
    val n = layout(orig)
    val transformed = n zip members map (x => fitToBounds(x._2, x._1))
    ElemList(transformed.toList)
  }

  def sequence[T <: Renderable[T], F: FC](members: Seq[T]): ElemList[T] =
    sequence(members, FreeLayout)

  def sequence[T <: Renderable[T], F: FC](
      members: Seq[Build[T]],
      layout: Layout
  ): Build[ElemList[T]] = {
    case (Some(old), e: Event) =>
      val members1 = (old.members zip members) map { case (old, build) =>
        build(Some(old), e)
      }
      sequence(members1, layout)
    case (None, BuildEvent) =>
      sequence(members.map(_.build), layout)
    case _ => throw new RuntimeException("should not happen")
  }

  def sequence[T <: Renderable[T], F: FC](
      members: Seq[Build[T]]
  ): Build[ElemList[T]] =
    sequence(members, FreeLayout)

  def sequence2[T1 <: Renderable[T1], T2 <: Renderable[T2], F: FC](
      members: Seq[Either[T1, T2]],
      layout: Layout
  ): ElemList2[T1, T2] = {
    val bounds = members.map(_.fold(_.bounds, _.bounds))

    val n = layout(bounds)

    val transformed = n zip members map (x =>
      x._2 match {
        case scala.util.Left(y) =>
          scala.util.Left(fitToBounds(y, x._1))
        case scala.util.Right(y) =>
          scala.util.Right(fitToBounds(y, x._1))
      }
    )
    ElemList2(transformed)
  }

  def sequence2[T1 <: Renderable[T1], T2 <: Renderable[T2], F: FC](
      members1: Seq[Either[Build[T1], Build[T2]]],
      layout: Layout
  ): Build[ElemList2[T1, T2]] = {
    case (None, BuildEvent) =>
      sequence2(
        members1.map(
          _.fold(x => scala.util.Left(x.build), x => scala.util.Right(x.build))
        ),
        layout
      )
    case (Some(old), e: Event) =>
      val members: Seq[Either[T1, T2]] = (old.members zip members1) map {
        case (old, build) =>
          build match {
            case scala.util.Left(x) =>
              scala.util.Left(x(Some(old.left.toOption.get) -> e))
            case scala.util.Right(x) =>
              scala.util.Right(x(Some(old.toOption.get) -> e))
          }
      }
      sequence2(members, layout)

    case _ => throw new RuntimeException("should not happen")
  }

  /* Normalized scientific notation. */
  private[nspl] def scientific(x: Double) =
    x / math.pow(10d, math.log10(x).round.toDouble) -> math
      .log10(x)
      .round
      .toDouble

  private[nspl] def mapPoint(p: Point, from: Bounds, to: Bounds, invertY: Boolean): Point =
    if (from.w == 0 || from.h == 0) Point(0d, 0d)
    else {
      val xF = to.w / from.w
      val yF = to.h / from.h
      Point(
        math.abs(p.x - from.x) * xF + to.x,
        if (!invertY) math.abs(p.y - from.y) * yF + to.y
        else math.abs(p.y - from.maxY) * yF + to.y
      )
    }

  val lineWidth : RelFontSize = 0.08 fts

  private[nspl] val defaultTickFormatter: Seq[Double] => Seq[String] =
    (worldCoordinates: Seq[Double]) => {
      if (worldCoordinates.isEmpty) Nil
      else {
        val range = worldCoordinates.max - worldCoordinates.min
        val precision = 4
        val r = worldCoordinates.map { w =>
          if ((math.abs(w) <= 1e-4 || math.abs(w) >= 1e4) && w != 0.0) {
            {
              val raw = (new java.util.Formatter)
                .format("%." + precision + "e", java.lang.Double.valueOf(w))
                .toString
              val sep = raw.split("e")
              sep(0).reverse
                .dropWhile(_ == '0')
                .dropWhile(_ == '.')
                .reverse + "e" + sep(1)
            }

          } else
            (new java.util.Formatter)
              .format("%." + precision + "f", java.lang.Double.valueOf(w))
              .toString
              .reverse
              .dropWhile(_ == '0')
              .dropWhile(_ == '.')
              .reverse
        }
        r
      }
    }
// if (w == 0.0) "0" else f"$w%.2g")
}
