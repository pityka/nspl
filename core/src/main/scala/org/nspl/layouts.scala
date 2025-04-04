package org.nspl

import Align._

/** A Layout which does nothing. */
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

private[nspl] object LayoutHelper {

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

  def alignToAnchors[F: FC](
      table: Seq[Seq[Bounds]],
      horizontalGap: RelFontSize,
      verticalGap: RelFontSize,
      alignRows: Boolean,
      alignCols: Boolean
  ) = {

    val horiz = HorizontalStack(Anchor, horizontalGap)
    val vertic = VerticalStack(Anchor, verticalGap)

    val rowTable = if (alignCols) transpose(table) else table
    val rows = rowTable.map(i => horiz.apply(i))
    val rowOutlines = rows.map(s => outline(s.iterator, anchor = None))
    val rowOutlines_moved = vertic.apply(rowOutlines)
    val yCoordinates = rows zip rowOutlines_moved zip rowOutlines flatMap {
      case ((row, rowbound), rowOutline) =>
        val diff = rowbound.y - rowOutline.y
        row.map { r =>
          r.y + diff
        }
    }

    val colTable = if (alignRows) transpose(table) else table
    val cols = colTable.map(i => vertic.apply(i))
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
}

/** A Layout which puts elements into rows.*/
case class TableLayout(
    columns: Int,
    horizontalGap: RelFontSize = 0.5 fts,
    verticalGap: RelFontSize = 0.5 fts
) extends Layout {

  def apply[F: FC](s: Seq[Bounds]) = {
    if (s.isEmpty) s
    else
      LayoutHelper.alignToAnchors(
        s.grouped(columns).toList,
        horizontalGap,
        verticalGap,
        true,
        false
      )
  }
}

/** A Layout which puts elements into columns.*/
case class ColumnLayout(
    numRows: Int,
    horizontalGap: RelFontSize = 0.5 fts,
    verticalGap: RelFontSize = 0.5 fts
) extends Layout {
  def apply[F: FC](s: Seq[Bounds]) = {
    if (s.isEmpty) s
    else
      LayoutHelper.alignToAnchors(
        s.grouped(numRows).toList,
        horizontalGap,
        verticalGap,
        false,
        true
      )
  }
}
