package org.nspl

sealed trait PathOperation {
  private[nspl] def extremities(last: Point): Seq[Point]
  private[nspl] def last: Point
  private[nspl] def transform(tx: AffineTransform): PathOperation
}
object PathOperation {
  case class MoveTo(p: Point) extends PathOperation {
    private[nspl] def extremities(last: Point) = Nil
    private[nspl] def last = p
    private[nspl] def transform(tx: AffineTransform) = MoveTo(tx.transform(p))
  }
  case class LineTo(p: Point) extends PathOperation {
    private[nspl] def extremities(last: Point) = List(last, p)
    private[nspl] def last = p
    private[nspl] def transform(tx: AffineTransform) = LineTo(tx.transform(p))
  }
  case class QuadTo(p2: Point, p1: Point) extends PathOperation {
    private[nspl] def last = p2
    private[nspl] def extremities(p0: Point) = {
      // x'(t) = 2 * (1-t) * (p0-p1) + 2*t(p2-p1) = 0
      // t = (p0-p1)/(p0-p2)
      val tx = (p0.x - p1.x) / (p0.x - p2.x)
      val ty = (p0.y - p1.y) / (p0.y - p2.y)

      p0 :: p2 :: (
        List(tx, ty)
          .filter(t => t >= 0d && t <= 1d)
          .map(t =>
            Point(
              (1 - t) * ((1 - t) * p0.x + t * p1.x) + t * ((1 - t) * p1.x + t * p2.x),
              (1 - t) * ((1 - t) * p0.y + t * p1.y) + t * ((1 - t) * p1.y + t * p2.y)
            )
          )
      )
    }
    private[nspl] def transform(tx: AffineTransform) =
      QuadTo(tx.transform(p1), tx.transform(p2))

  }
  case class CubicTo(p3: Point, p1: Point, p2: Point) extends PathOperation {
    private[nspl] def last = p3
    private[nspl] def extremities(p0: Point) = {
      val t1x = (math.sqrt(
        -1 * p0.x * p2.x + p0.x * p3.x + p1.x * p1.x - p1.x * p2.x - p1.x * p3.x + p2.x * p2.x
      ) - p0.x + 2 * p1.x - p2.x) / (-1 * p0.x + 3 * p1.x - 3 * p2.x + p3.x)

      val t2x = -1 * (math.sqrt(
        -1 * p0.x * p2.x + p0.x * p3.x + p1.x * p1.x - p1.x * p2.x - p1.x * p3.x + p2.x * p2.x
      ) + p0.x - 2 * p1.x + p2.x) / (-1 * p0.x + 3 * p1.x - 3 * p2.x + p3.x)

      val t1y = (math.sqrt(
        -1 * p0.y * p2.y + p0.y * p3.y + p1.y * p1.y - p1.y * p2.y - p1.y * p3.y + p2.y * p2.y
      ) - p0.y + 2 * p1.y - p2.y) / (-1 * p0.y + 3 * p1.y - 3 * p2.y + p3.y)

      val t2y = -1 * (math.sqrt(
        -1 * p0.y * p2.y + p0.y * p3.y + p1.y * p1.y - p1.y * p2.y - p1.y * p3.y + p2.y * p2.y
      ) + p0.y - 2 * p1.y + p2.y) / (-1 * p0.y + 3 * p1.y - 3 * p2.y + p3.y)

      val t3x = (2 * p1.x - 3 * p2.x + p3.x) / (2 * p1.x - 2 * p2.x + p3.x)
      val t3y = (2 * p1.y - 3 * p2.y + p3.y) / (2 * p1.y - 2 * p2.y + p3.y)

      p0 :: p3 :: (List(t1x, t2x, t1y, t2y, t3x, t3y)
        .filter(t => 0.0 >= t && t <= 1.0)
        .map(t =>
          Point(
            (1 - t) * (1 - t) * (1 - t) * p0.x + 3 * (1 - t) * (1 - t) * t * p1.x + 3 * (1 - t) * t * t * p2.x + t * t * t * p3.x,
            (1 - t) * (1 - t) * (1 - t) * p0.y + 3 * (1 - t) * (1 - t) * t * p1.y + 3 * (1 - t) * t * t * p2.y + t * t * t * p3.y
          )
        ))
        .filterNot(p => p.x.isNaN() || p.y.isNaN())

    }
    private[nspl] def transform(tx: AffineTransform) =
      CubicTo(tx.transform(p3), tx.transform(p1), tx.transform(p2))
  }
}

/** A shape built up by a path
* 
* A path is a sequence of of path operations:
* - move to point
* - line to point (from last point)
* - quadratic to (from last point)
* - cubic to (from last point)
*/
case class Path(path: Seq[PathOperation], currentTransform: AffineTransform)
    extends Shape {
  val bounds: Bounds = {
    val extrema =
      path.sliding(2).toList.flatMap { pair =>
        val first = pair(0)
        val second = pair(1)
        second.extremities(first.last)
      }

    val minx = extrema.map(_.x).min
    val maxx = extrema.map(_.x).max

    val miny = extrema.map(_.y).min
    val maxy = extrema.map(_.y).max

    Bounds(minx, miny, maxx - minx, maxy - miny)

  }

  def transform(tx: (Bounds, AffineTransform) => AffineTransform) =
    this.copy(currentTransform = tx(bounds, this.currentTransform))

  def transform(tx: AffineTransform) =
    this.copy(currentTransform = tx.applyBefore(this.currentTransform))

}
