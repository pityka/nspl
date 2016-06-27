package org.nspl

trait Shape {
  def bounds: Bounds
  def transform(tx: Bounds => AffineTransform): Shape
}

case class Rectangle(
    x: Double,
    y: Double,
    w: Double,
    h: Double,
    tx: AffineTransform = AffineTransform.identity
) extends Shape {
  def bounds = tx.transform(Bounds(x, y, w, h))
  def transform(tx: Bounds => AffineTransform) = this.copy(tx = tx(bounds).concat(this.tx))
}

case class Ellipse(
    x: Double,
    y: Double,
    w: Double,
    h: Double,
    tx: AffineTransform = AffineTransform.identity
) extends Shape {
  def bounds = tx.transform(Bounds(x, y, w, h))
  def transform(tx: Bounds => AffineTransform) = this.copy(tx = tx(bounds).concat(this.tx))
}

case class Line(x1: Double, y1: Double, x2: Double, y2: Double) extends Shape {
  def bounds = Bounds(math.min(x1, x2), math.min(y1, y2), math.abs(x1 - x2), math.abs(y1 - y2))
  def transform(tx: Bounds => AffineTransform) = {
    val tx1 = tx(bounds)
    val p1 = tx1.transform(Point(x1, y1))
    val p2 = tx1.transform(Point(x2, y2))
    Line(p1.x, p1.y, p2.x, p2.y)
  }
}

/* Path without curves. */
case class SimplePath(ps: Seq[Point]) extends Shape {

  def bounds = {
    val minx = ps.map(_.x).min
    val maxx = ps.map(_.x).max

    val miny = ps.map(_.y).min
    val maxy = ps.map(_.y).max

    Bounds(minx, miny, maxx - minx, maxy - miny)
  }

  def transform(tx: Bounds => AffineTransform) = {
    val tx1 = tx(bounds)
    SimplePath(ps.map(p => tx1.transform(p)))
  }

}

object Shape {
  def line(p1: Point, p2: Point) = Line(p1.x, p1.y, p2.x, p2.y)
  def rectangle(x: Double, y: Double, w: Double, h: Double) = Rectangle(x, y, w, h)
  def ellipse(x: Double, y: Double, w: Double, h: Double) = Ellipse(x, y, w, h)
  def circle(r: Double) = Ellipse(-1.0 * r / 2.0, -1.0 * r / 2.0, r, r)
  def square(r: Double) = Rectangle(-1.0 * r / 2.0, -1.0 * r / 2.0, r, r)
}
