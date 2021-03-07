package org.nspl

sealed trait PathOperation {
  def extremities(last: Point): Seq[Point]
  def last: Point
  def transform(tx: AffineTransform): PathOperation
}
case class MoveTo(p: Point) extends PathOperation {
  def extremities(last: Point) = Nil
  def last = p
  def transform(tx: AffineTransform) = MoveTo(tx.transform(p))
}
case class LineTo(p: Point) extends PathOperation {
  def extremities(last: Point) = List(last, p)
  def last = p
  def transform(tx: AffineTransform) = LineTo(tx.transform(p))
}
case class QuadTo(p2: Point, p1: Point) extends PathOperation {
  def last = p2
  def extremities(p0: Point) = {
    // x'(t) = 2 * (1-t) * (p0-p1) + 2*t(p2-p1) = 0
    // t = (p0-p1)/(p0-p2)
    val tx = (p0.x - p1.x) / (p0.x - p2.x)
    val ty = (p0.y - p1.y) / (p0.y - p2.y)

    p0 :: p2 :: (List(tx, ty)
      .filter(t => t >= 0d && t <= 1d)
      .map(
        t =>
          Point(
            (1 - t) * ((1 - t) * p0.x + t * p1.x) + t * ((1 - t) * p1.x + t * p2.x),
            (1 - t) * ((1 - t) * p0.y + t * p1.y) + t * ((1 - t) * p1.y + t * p2.y)
          )
      ))
  }
  def transform(tx: AffineTransform) =
    QuadTo(tx.transform(p1), tx.transform(p2))

}
case class CubicTo(p3: Point, p1: Point, p2: Point) extends PathOperation {
  def last = p3
  def extremities(p0: Point) = {
    // x'(t) = 3 * (1-t)^2 (p1-p0)+6*(1-t)*t*(p2-p1)+3*t^2*(p3-p2) = 0
    // x''(t) = 6 * (1-t) * (p2-2*p1+p0) + 6 *t* (p3-2p2+p1)
    val t1x = (-1 * math.sqrt(
      -1 * p0.x * p2.x + p0.x * p3.x + p1.x * p1.x - p1.x * p2.x - p1.x * p3.x + p2.x * p2.x
    ) - p0.x + 2 * p1.x - p2.x) / (-1 * p0.x + 3 * p1.x - 3 * p2.x + p3.x)
    val t2x = -1 * (math.sqrt(
      -1 * p0.x * p2.x + p0.x * p3.x + p1.x * p1.x - p1.x * p2.x - p1.x * p3.x + p2.x * p2.x
    ) - p0.x + 2 * p1.x - p2.x) / (-1 * p0.x + 3 * p1.x - 3 * p2.x + p3.x)

    val t1y = (-1 * math.sqrt(
      -1 * p0.y * p2.y + p0.y * p3.y + p1.y * p1.y - p1.y * p2.y - p1.y * p3.y + p2.y * p2.y
    ) - p0.y + 2 * p1.y - p2.y) / (-1 * p0.y + 3 * p1.y - 3 * p2.y + p3.y)
    val t2y = -1 * (math.sqrt(
      -1 * p0.y * p2.y + p0.y * p3.y + p1.y * p1.y - p1.y * p2.y - p1.y * p3.y + p2.y * p2.y
    ) - p0.y + 2 * p1.y - p2.y) / (-1 * p0.y + 3 * p1.y - 3 * p2.y + p3.y)

    val t3x = (2 * p1.x - 3 * p2.x + p3.x) / (2 * (p1.x - 2 * p2.x + p3.x))
    val t3y = (2 * p1.y - 3 * p2.y + p3.y) / (2 * (p1.y - 2 * p2.y + p3.y))

    (p0 :: p3 :: (List(t1x, t2x, t1y, t2y, t3x, t3y)
      .filter(t => t >= 0d && t <= 1d)
      .map(
        t =>
          Point(
            (1 - t) * (1 - t) * (1 - t) * p0.x + 3 * (1 - t) * (1 - t) * t * p1.x + 3 * (1 - t) * t * t * p2.x + t * t * t * p3.x,
            (1 - t) * (1 - t) * (1 - t) * p0.y + 3 * (1 - t) * (1 - t) * t * p1.y + 3 * (1 - t) * t * t * p2.y + t * t * t * p3.y
          )
      ))).filterNot(v => v.x.isNaN || v.y.isNaN)

  }
  def transform(tx: AffineTransform) =
    CubicTo(tx.transform(p1), tx.transform(p2), tx.transform(p3))
}

case class Path(path: Seq[PathOperation]) extends Shape {
  def bounds: Bounds = {
    val extrema =
      path.sliding(2).toList.flatMap(g => g(1).extremities(g(0).last))

    val minx = extrema.map(_.x).min
    val maxx = extrema.map(_.x).max

    val miny = extrema.map(_.y).min
    val maxy = extrema.map(_.y).max

    Bounds(minx, miny, maxx - minx, maxy - miny)

  }
  def transform(tx: Bounds => AffineTransform) = {
    val t = tx(bounds)
    Path(path.map(_.transform(t)))
  }
}
