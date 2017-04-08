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

  trait Event { self =>
    def mapBounds(from: Bounds, to: Bounds) = self
  }
  case class Click(point: Point) extends Event {
    override def mapBounds(from: Bounds, to: Bounds) = Click(mapPoint(point, from, to))
  }
  case class Scroll(v: Double, location: Point) extends Event {
    override def mapBounds(from: Bounds, to: Bounds) = Scroll(v, mapPoint(location, from, to))
  }
  case object BuildEvent extends Event

}
