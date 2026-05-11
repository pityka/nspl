package org.nspl

/* */
object Build {

  def apply[A](f: A)(pf: PartialFunction[(Option[A], Event), A]): Build[A] = {
    case x => pf.applyOrElse(x, (x: (Option[A], Event)) => x._1.getOrElse(f))
  }

  def const[A](f: => A): Build[A] = {
    val pf: PartialFunction[Event, A] = { case _ => f }
    withoutState(pf)
  }
  def withoutState[A](pf: PartialFunction[Event, A]): Build[A] = {
    case (_, e) => pf(e)
  }
}

private[nspl] trait Events {

  /** An event born fro user interaction */
  trait Event

  /** A scroll event over a plot area.
    *
    * @param plotArea
    *   identifies which plot area is being scrolled. The bounds member of the
    *   identifier must be defined.
    */
  case class Scroll(v: Double, location: Point, plotArea: PlotAreaIdentifier)
      extends Event

  /** A drag event over a plot area.
    *
    * @param plotArea
    *   identifies which plot area is being dragged. The bounds member of the
    *   identifier must be defined.
    */
  case class Drag(start: Point, current: Point, plotArea: PlotAreaIdentifier)
      extends Event

  /** A rectangular selection (shift+drag) over a plot area. `start` and
    * `current` are the two corners in canvas coordinates.
    *
    * @param plotArea
    *   identifies which plot area was selected. The bounds member of the
    *   identifier must be defined.
    */
  case class Selection(
      start: Point,
      current: Point,
      plotArea: PlotAreaIdentifier
  ) extends Event

  /* The event representing the first build (before any user interaction happened) of component */
  case object BuildEvent extends Event

}

/** Buffers a sequence of events that, when replayed against a fresh `Build`,
  * reproduces the current view. Consecutive Drag or Selection events on the
  * same plot area are fused — that's the "fusion helper" part of the name —
  * so the replay log stays compact (one cumulative drag rather than N
  * incremental ones).
  *
  * This is consumed by the canvas backend's interactive `render`. It is
  * exposed in core (rather than canvas) so it can be unit-tested on the JVM
  * — its logic is pure Scala and does not depend on any DOM types.
  */
class EventFusionHelper {
  private var buffer: Vector[Event] = Vector.empty

  def clear(): Unit = buffer = Vector.empty
  def get: Vector[Event] = buffer
  def size: Int = buffer.size

  def add(ev: Event, fusable: Boolean): Unit =
    if (!fusable) buffer = buffer :+ ev
    else
      (buffer.lastOption, ev) match {
        case (Some(Drag(s1, c1, a1)), Drag(s2, c2, a2))
            if a1.id == a2.id && c1 == s2 =>
          // consecutive incremental drags: collapse to the cumulative drag
          buffer = buffer.dropRight(1) :+ Drag(s1, c2, a1)
        case (Some(Drag(s1, _, a1)), Drag(s2, c2, a2))
            if a1.id == a2.id && s1 == s2 =>
          // same start point, growing endpoint: keep the latest endpoint
          buffer = buffer.dropRight(1) :+ Drag(s1, c2, a1)
        case (Some(Selection(s1, _, a1)), Selection(s2, c2, a2))
            if a1.id == a2.id && s1 == s2 =>
          // selection growing from a fixed corner
          buffer = buffer.dropRight(1) :+ Selection(s1, c2, a1)
        case _ =>
          buffer = buffer :+ ev
      }
}
