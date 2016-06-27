package org.nspl

sealed trait Alignment
case object Right extends Alignment
case object Center extends Alignment
case object Left extends Alignment

/* A Layout which does nothing. */
object FreeLayout extends Layout {
  def apply(s: Seq[Bounds]) = s
}

/**
 * A Layout which stacks elements on top of each other
 *  and aligns the horizontal axis.
 */
case class VerticalStack(alignment: Alignment = Left, gap: Double = 0.0) extends Layout {
  def apply(s: Seq[Bounds]) = {
    if (s.isEmpty) Nil
    else {
      val maxWidth = s.map(_.w).max
      val minX = s.map(_.x).min
      def xpos(width: Double) = {
        alignment match {
          case Left => 0.0 + minX
          case Right => maxWidth - width + minX
          case Center => 0.5 * maxWidth - 0.5 * width + minX
        }
      }
      s.foldLeft(s.map(_.y).min -> Seq[Bounds]()) {
        case ((y, seq), bounds) =>
          (y + bounds.h + gap, seq :+ Bounds(xpos(bounds.w), y, bounds.w, bounds.h))
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
      val maxHeight = s.map(_.h).max
      val minY = s.map(_.y).min
      def ypos(height: Double) = {
        alignment match {
          case Left => 0.0 + minY
          case Right => maxHeight - height + minY
          case Center => 0.5 * maxHeight - 0.5 * height + minY
        }
      }
      s.foldLeft(s.map(_.x).min -> List[Bounds]()) {
        case ((x, seq), elem) =>
          (x + elem.w + gap, Bounds(x, ypos(elem.h), elem.w, elem.h) :: seq)
      }
        ._2.reverse
    }
  }
}

/* A Layout which puts elements into rows.*/
case class TableLayout(columns: Int) extends Layout {
  val horiz = HorizontalStack(Left, 10d)
  val vertic = VerticalStack(Center, 10d)
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
