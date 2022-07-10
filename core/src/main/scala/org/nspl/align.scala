package org.nspl

sealed trait Alignment
object Align {
  case object Right extends Alignment
  case object Center extends Alignment
  case object Left extends Alignment
  case object NoAlignment extends Alignment
  case object Anchor extends Alignment


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
  def horizontalGapAfterReference[T <: Renderable[T]](
      move: T,
      reference: Bounds,
      gap: Double
  ): T =
    fitToBounds(
      move,
      Bounds(
        reference.x + reference.w + gap,
        move.bounds.y,
        move.bounds.w,
        move.bounds.h
      )
    )

  def horizontalGapBeforeReference[T <: Renderable[T]](
      move: T,
      reference: Bounds,
      gap: Double
  ): T =
    fitToBounds(
      move,
      Bounds(
        reference.x - gap - move.bounds.w,
        move.bounds.y,
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
