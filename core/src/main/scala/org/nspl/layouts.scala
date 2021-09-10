package org.nspl

sealed trait Alignment
case object Right extends Alignment
case object Center extends Alignment
case object Left extends Alignment
case object NoAlignment extends Alignment
case object Anchor extends Alignment

/* A Layout which does nothing. */
object FreeLayout extends Layout {
  def apply[F: FC](s: Seq[Bounds]) = s
}

case class RelativeToFirst(x: Double, y: Double) extends Layout {
  def apply[F: FC](s: Seq[Bounds]) = {
    if (s.isEmpty) Nil
    else {
      val first = s.head
      first +: s.drop(1).map { b =>
        Bounds(
          first.x + x,
          first.y + y,
          b.w,
          b.h,
          b.anchor.map(_.translate(b.x - first.x - x, b.y - first.y - y))
        )
      }
    }
  }
}

/** A Layout which stacks elements on top of each other and aligns the
  * horizontal axis.
  */
case class VerticalStack(
    alignment: Alignment = Left,
    gap: RelFontSize = 0.0 fts
) extends Layout {
  def apply[F: FC](s: Seq[Bounds]) = {
    if (s.isEmpty) Nil
    else {
      val maxWidthElem = s.maxBy(_.w)
      val maxWidth = maxWidthElem.w
      val minX = maxWidthElem.x
      val referenceAnchorX =
        if (s.forall(_.anchor.isDefined)) maxWidthElem.anchor.get.x
        else minX
      def xpos(width: Double, x: Double, anchor: Option[Point]) = {
        alignment match {
          case Left        => 0.0 + minX
          case Right       => maxWidth - width + minX
          case Center      => 0.5 * maxWidth - 0.5 * width + minX
          case NoAlignment => x
          case Anchor =>
            val a = anchor.map(_.x).getOrElse(x)
            val shift = a - x
            referenceAnchorX - shift
        }
      }
      s.foldLeft(s.map(_.y).min -> Seq[Bounds]()) { case ((y, seq), bounds) =>
        val xp = xpos(bounds.w, bounds.x, bounds.anchor)
        (
          y + bounds.h + gap.value,
          seq :+ Bounds(
            xp,
            y,
            bounds.w,
            bounds.h,
            bounds.anchor.map(_.translate(xp - bounds.x, y - bounds.y))
          )
        )
      }._2
    }
  }
}

/** A Layout which stacks elements beside each other and aligns the vertical
  * axis.
  */
case class HorizontalStack(alignment: Alignment, gap: RelFontSize = 0.0 fts)
    extends Layout {
  def apply[F: FC](s: Seq[Bounds]) = {
    if (s.isEmpty) Nil
    else {
      val maxHeightElem = s.maxBy(_.h)
      val maxHeight = maxHeightElem.h
      val minY = maxHeightElem.y
      val referenceAnchorY =
        if (s.forall(_.anchor.isDefined)) maxHeightElem.anchor.get.y else minY
      def ypos(height: Double, y: Double, anchor: Option[Point]) = {
        alignment match {
          case Left        => 0.0 + minY
          case Right       => maxHeight - height + minY
          case Center      => 0.5 * maxHeight - 0.5 * height + minY
          case NoAlignment => y
          case Anchor =>
            val a = anchor.map(_.y).getOrElse(y)
            val shift = a - y
            referenceAnchorY - shift
        }
      }
      s.foldLeft(s.map(_.x).min -> List[Bounds]()) { case ((x, seq), elem) =>
        val yp = ypos(elem.h, elem.y, elem.anchor)
        (
          x + elem.w + gap.value,
          Bounds(
            x,
            yp,
            elem.w,
            elem.h,
            elem.anchor.map(_.translate(x - elem.x, yp - elem.y))
          ) :: seq
        )
      }._2
        .reverse
    }
  }
}

object LayoutHelper {

  def transpose[A](a: Seq[Seq[A]]) = {
    if (a.isEmpty) a
    else {
      val max = a.map(_.size).max
      val uniform =
        a.map { aa =>
          aa ++ List.fill(max - aa.size)(null.asInstanceOf[A])
        }
      uniform.transpose.map(_.filterNot(_ == null))
    }
  }

  def alignRowsToAnchors[F: FC](
      table: Seq[Seq[Bounds]],
      horizontalGap: RelFontSize,
      verticalGap: RelFontSize
  ) = {

    val horiz = HorizontalStack(Anchor, horizontalGap)
    val vertic = VerticalStack(Anchor, verticalGap)
    val rows = table.map(i => horiz.apply(i))
    val rowOutlines = rows.map(s => outline(s.iterator, anchor = None))
    val rowOutlines_moved = vertic.apply(rowOutlines)
    val yCoordinates = rows zip rowOutlines_moved zip rowOutlines flatMap {
      case ((row, rowbound), rowOutline) =>
        val diff = rowbound.y - rowOutline.y
        row.map { r =>
          r.y + diff
        }
    }

    val cols = transpose(table).map(i => vertic.apply(i))
    val columnOutlines = cols.map(s => outline(s.iterator, anchor = None))
    val columnOutlines_moved = horiz.apply(columnOutlines)
    val xCoordinates = cols zip columnOutlines_moved zip columnOutlines map {
      case ((col, colbound), colOutline) =>
        val diff = colbound.x - colOutline.x
        col.map { c =>
          c.x + diff
        }
    }
    (transpose(xCoordinates).flatten zip yCoordinates zip table.flatten).map {
      case ((c, r), b) =>
        Bounds(c, r, b.w, b.h, b.anchor)
    }
  }
  def alignColumnsToAnchors[F: FC](
      table: Seq[Seq[Bounds]],
      horizontalGap: RelFontSize,
      verticalGap: RelFontSize
  ) = {

    val horiz = HorizontalStack(Anchor, horizontalGap)
    val vertic = VerticalStack(Anchor, verticalGap)
    val cols = table.map(i => vertic.apply(i))
    val colOutlines = cols.map(s => outline(s.iterator, anchor = None))
    val colOutlines_moved = horiz.apply(colOutlines)
    val xCoordinates = cols zip colOutlines_moved zip colOutlines flatMap {
      case ((col, colbound), colOutline) =>
        val diff = colbound.x - colOutline.x
        col.map { c =>
          c.x + diff
        }
    }

    val rows = transpose(table).map(i => horiz.apply(i))
    val rowOutlines = rows.map(s => outline(s.iterator, anchor = None))
    val rowOutlines_moved = vertic.apply(rowOutlines)
    val yCoordinates = rows zip rowOutlines_moved zip rowOutlines map {
      case ((row, rowbound), rowOutline) =>
        val diff = rowbound.y - rowOutline.y
        row.map { r =>
          r.y + diff
        }
    }
    (transpose(yCoordinates).flatten zip xCoordinates zip table.flatten).map {
      case ((r, c), b) =>
        Bounds(c, r, b.w, b.h, b.anchor)
    }
  }
}

/* A Layout which puts elements into rows.*/
case class TableLayout(
    columns: Int,
    horizontalGap: RelFontSize = 0.5 fts,
    verticalGap: RelFontSize = 0.5 fts
) extends Layout {

  def apply[F: FC](s: Seq[Bounds]) = {
    if (s.isEmpty) s
    else
      LayoutHelper.alignRowsToAnchors(
        s.grouped(columns).toList,
        horizontalGap,
        verticalGap
      )
  }
}

/* A Layout which puts elements into columns.*/
case class ColumnLayout(
    numRows: Int,
    horizontalGap: RelFontSize = 0.5 fts,
    verticalGap: RelFontSize = 0.5 fts
) extends Layout {
  def apply[F: FC](s: Seq[Bounds]) = {
    if (s.isEmpty) s
    else
      LayoutHelper.alignColumnsToAnchors(
        s.grouped(numRows).toList,
        horizontalGap,
        verticalGap
      )
  }
}

object AlignTo {

  def horizontal[T <: Renderable[T]](
      move: T,
      reference: Bounds,
      alignment: Alignment
  ): T =
    alignment match {
      case Left        => horizontalLeft(move, reference)
      case Right       => horizontalRight(move, reference)
      case Center      => horizontalCenter(move, reference)
      case NoAlignment => move
      case Anchor      => horizontalAnchor(move, reference)
    }

  def vertical[T <: Renderable[T]](
      move: T,
      reference: Bounds,
      alignment: Alignment
  ): T = alignment match {
    case Left        => verticalLeft(move, reference)
    case Right       => verticalRight(move, reference)
    case Center      => verticalCenter(move, reference)
    case NoAlignment => move
    case Anchor      => verticalAnchor(move, reference)
  }

  def horizontalRight[T <: Renderable[T]](move: T, reference: Bounds): T =
    fitToBounds(
      move,
      Bounds(
        reference.x + reference.w - move.bounds.w,
        move.bounds.y,
        move.bounds.w,
        move.bounds.h
      )
    )
  def horizontalLeft[T <: Renderable[T]](move: T, reference: Bounds): T =
    fitToBounds(
      move,
      Bounds(reference.x, move.bounds.y, move.bounds.w, move.bounds.h)
    )
  def horizontalCenter[T <: Renderable[T]](move: T, reference: Bounds): T =
    fitToBounds(
      move,
      Bounds(
        reference.x + (reference.w - move.bounds.w) * 0.5,
        move.bounds.y,
        move.bounds.w,
        move.bounds.h
      )
    )
  def horizontalAnchor[T <: Renderable[T]](move: T, reference: Bounds): T =
    fitToBounds(
      move,
      Bounds(
        reference.anchor
          .map(_.x)
          .getOrElse(reference.x) + (move.bounds.x - move.bounds.anchor
          .map(_.x)
          .getOrElse(move.bounds.x)),
        move.bounds.y,
        move.bounds.w,
        move.bounds.h
      )
    )

  def verticalRight[T <: Renderable[T]](move: T, reference: Bounds): T =
    fitToBounds(
      move,
      Bounds(
        move.bounds.x,
        reference.y + reference.h - move.bounds.h,
        move.bounds.w,
        move.bounds.h
      )
    )

  def verticalAnchor[T <: Renderable[T]](move: T, reference: Bounds): T =
    fitToBounds(
      move,
      Bounds(
        move.bounds.x,
        reference.anchor
          .map(_.y)
          .getOrElse(reference.y) + (move.bounds.y - move.bounds.anchor
          .map(_.y)
          .getOrElse(move.bounds.y)),
        move.bounds.w,
        move.bounds.h
      )
    )

  def verticalGapAfterReference[T <: Renderable[T]](
      move: T,
      reference: Bounds,
      gap: Double
  ): T =
    fitToBounds(
      move,
      Bounds(
        move.bounds.x,
        reference.y + reference.h + gap,
        move.bounds.w,
        move.bounds.h
      )
    )

  def verticalGapBeforeReference[T <: Renderable[T]](
      move: T,
      reference: Bounds,
      gap: Double
  ): T =
    fitToBounds(
      move,
      Bounds(
        move.bounds.x,
        reference.y - gap - move.bounds.h,
        move.bounds.w,
        move.bounds.h
      )
    )
  def verticalLeft[T <: Renderable[T]](move: T, reference: Bounds): T =
    fitToBounds(
      move,
      Bounds(move.bounds.x, reference.y, move.bounds.w, move.bounds.h)
    )
  def verticalCenter[T <: Renderable[T]](move: T, reference: Bounds): T =
    fitToBounds(
      move,
      Bounds(
        move.bounds.x,
        reference.y + (reference.h - move.bounds.h) * 0.5,
        move.bounds.w,
        move.bounds.h
      )
    )

  def center[T <: Renderable[T]](move: T, reference: Bounds): T =
    fitToBounds(
      move,
      Bounds(
        reference.x + (reference.w - move.bounds.w) * 0.5,
        reference.y + (reference.h - move.bounds.h) * 0.5,
        move.bounds.w,
        move.bounds.h
      )
    )

  def topLeftCorner[T <: Renderable[T]](move: T, reference: Bounds): T =
    verticalLeft(horizontalLeft(move, reference), reference)

  def topRightCorner[T <: Renderable[T]](move: T, reference: Bounds): T =
    verticalLeft(horizontalRight(move, reference), reference)

  def topCenter[T <: Renderable[T]](move: T, reference: Bounds): T =
    verticalLeft(horizontalCenter(move, reference), reference)

  def bottomLeftCorner[T <: Renderable[T]](move: T, reference: Bounds): T =
    verticalRight(horizontalLeft(move, reference), reference)

  def bottomRightCorner[T <: Renderable[T]](move: T, reference: Bounds): T =
    verticalRight(horizontalRight(move, reference), reference)

  def bottomCenter[T <: Renderable[T]](move: T, reference: Bounds): T =
    verticalRight(horizontalCenter(move, reference), reference)

  def centerLeft[T <: Renderable[T]](move: T, reference: Bounds): T =
    verticalCenter(horizontalLeft(move, reference), reference)

  def centerRight[T <: Renderable[T]](move: T, reference: Bounds): T =
    verticalCenter(horizontalRight(move, reference), reference)

}
