package org.nspl

sealed trait Alignment
case object Right extends Alignment
case object Center extends Alignment
case object Left extends Alignment
case object NoAlignment extends Alignment

/* A Layout which does nothing. */
object FreeLayout extends Layout {
  def apply(s: Seq[Bounds]) = s
}

case class RelativeToFirst(x: Double, y: Double) extends Layout {
  def apply(s: Seq[Bounds]) = {
    if (s.isEmpty) Nil
    else {
      val first = s.head
      first +: s.drop(1).map { b =>
        Bounds(first.x + x, first.y + y, b.w, b.h)
      }
    }
  }
}

/**
 * A Layout which stacks elements on top of each other
 *  and aligns the horizontal axis.
 */
case class VerticalStack(alignment: Alignment = Left, gap: Double = 0.0) extends Layout {
  def apply(s: Seq[Bounds]) = {
    if (s.isEmpty) Nil
    else {
      val maxWidthElem = s.maxBy(_.w)
      val maxWidth = maxWidthElem.w
      val minX = maxWidthElem.x
      def xpos(width: Double, x: Double) = {
        alignment match {
          case Left => 0.0 + minX
          case Right => maxWidth - width + minX
          case Center => 0.5 * maxWidth - 0.5 * width + minX
          case NoAlignment => x
        }
      }
      s.foldLeft(s.map(_.y).min -> Seq[Bounds]()) {
        case ((y, seq), bounds) =>
          (y + bounds.h + gap, seq :+ Bounds(xpos(bounds.w, bounds.x), y, bounds.w, bounds.h))
      }
        ._2
    }
  }
}

/**
 * A Layout which stacks elements beside each other
 *  and aligns the vertical axis.
 */
case class HorizontalStack(alignment: Alignment, gap: Double = 0.0) extends Layout {
  def apply(s: Seq[Bounds]) = {
    if (s.isEmpty) Nil
    else {
      val maxHeightElem = s.maxBy(_.h)
      val maxHeight = maxHeightElem.h
      val minY = maxHeightElem.y
      def ypos(height: Double, y: Double) = {
        alignment match {
          case Left => 0.0 + minY
          case Right => maxHeight - height + minY
          case Center => 0.5 * maxHeight - 0.5 * height + minY
          case NoAlignment => y
        }
      }
      s.foldLeft(s.map(_.x).min -> List[Bounds]()) {
        case ((x, seq), elem) =>
          (x + elem.w + gap, Bounds(x, ypos(elem.h, elem.y), elem.w, elem.h) :: seq)
      }
        ._2.reverse
    }
  }
}

/* A Layout which puts elements into rows.*/
case class TableLayout(columns: Int, horizontalGap: Double = 10d, verticalGap: Double = 10d) extends Layout {
  val horiz = HorizontalStack(Left, horizontalGap)
  val vertic = VerticalStack(Center, verticalGap)
  def apply(s: Seq[Bounds]) = {
    val rows = s.grouped(columns).toList.map(i => horiz.apply(i))
    val outlines = rows.map(s => outline(s))
    val page = vertic.apply(outlines)
    rows zip page flatMap {
      case (row, rowbound) =>
        row.map { r =>
          Bounds(r.x, rowbound.y, r.w, r.h)
        }
    }
  }
}

/* A Layout which puts elements into columns.*/
case class ColumnLayout(numRows: Int) extends Layout {
  val horiz = HorizontalStack(Left, 10d)
  val vertic = VerticalStack(Center, 10d)
  def apply(s: Seq[Bounds]) = {
    val cols = s.grouped(numRows).toList.map(i => vertic.apply(i))
    val outlines = cols.map(s => outline(s))
    val page = horiz.apply(outlines)
    cols zip page flatMap {
      case (col, colbound) =>
        col.map { r =>
          Bounds(colbound.x, r.y, r.w, r.h)
        }
    }
  }
}

object AlignTo {

  def horizontal[T <: Renderable[T]](move: T, reference: Bounds, alignment: Alignment): T = alignment match {
    case Left => horizontalLeft(move, reference)
    case Right => horizontalRight(move, reference)
    case Center => horizontalCenter(move, reference)
    case NoAlignment => move
  }

  def vertical[T <: Renderable[T]](move: T, reference: Bounds, alignment: Alignment): T = alignment match {
    case Left => verticalLeft(move, reference)
    case Right => verticalRight(move, reference)
    case Center => verticalCenter(move, reference)
    case NoAlignment => move
  }

  def horizontalRight[T <: Renderable[T]](move: T, reference: Bounds): T = fitToBounds(move, Bounds(reference.x + reference.w - move.bounds.w, move.bounds.y, move.bounds.w, move.bounds.h))
  def horizontalLeft[T <: Renderable[T]](move: T, reference: Bounds): T = fitToBounds(move, Bounds(reference.x, move.bounds.y, move.bounds.w, move.bounds.h))
  def horizontalCenter[T <: Renderable[T]](move: T, reference: Bounds): T = fitToBounds(move, Bounds(reference.x + (reference.w - move.bounds.w) * 0.5, move.bounds.y, move.bounds.w, move.bounds.h))

  def verticalRight[T <: Renderable[T]](move: T, reference: Bounds): T = fitToBounds(move, Bounds(move.bounds.x, reference.y + reference.h - move.bounds.h, move.bounds.w, move.bounds.h))
  def verticalLeft[T <: Renderable[T]](move: T, reference: Bounds): T = fitToBounds(move, Bounds(move.bounds.x, reference.y, move.bounds.w, move.bounds.h))
  def verticalCenter[T <: Renderable[T]](move: T, reference: Bounds): T = fitToBounds(move, Bounds(move.bounds.x, reference.y + (reference.h - move.bounds.h) * 0.5, move.bounds.w, move.bounds.h))

  def center[T <: Renderable[T]](move: T, reference: Bounds): T = fitToBounds(move, Bounds(reference.x + (reference.w - move.bounds.w) * 0.5, reference.y + (reference.h - move.bounds.h) * 0.5, move.bounds.w, move.bounds.h))

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
