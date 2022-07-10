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

  /* The event representing the first build (before any user interaction happened) of component */
  case object BuildEvent extends Event

}
