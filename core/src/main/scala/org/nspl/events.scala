package org.nspl

object Build {
  def apply[A](f: => A)(pf: PartialFunction[(Option[A], Event), A]): Build[A] =
    { case x => pf.applyOrElse(x, (x: (Option[A], Event)) => f) }

  def const[A](f: => A): Build[A] = {
    val pf: PartialFunction[Event, A] = { case _ => f }
    withoutState(pf)
  }
  def withState[A](pf: PartialFunction[(Option[A], Event), A]): Build[A] = pf
  def withoutState[A](pf: PartialFunction[Event, A]): Build[A] = { case (o, e) => pf(e) }
}

trait Events {

  trait Event
  case class Click(point: Point) extends Event
  case class Scroll(v: Double, location: Point) extends Event
  case class Drag(start: Point, current: Point) extends Event
  case object BuildEvent extends Event

}
