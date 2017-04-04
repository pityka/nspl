package org.nspl

object Build {
  def apply[A](pf: PartialFunction[Event, A]): Build[A] = pf
  def const[A](f: => A): Build[A] = apply[A]({
    case _ => f
  })
}

trait Events {

  trait Event { self =>
    def mapBounds(from: Bounds, to: Bounds) = self
  }
  case class Click(point: Point) extends Event {
    override def mapBounds(from: Bounds, to: Bounds) = Click(mapPoint(point, from, to))
  }
  case class Scroll(v: Double) extends Event
  case object BuildEvent extends Event

}
