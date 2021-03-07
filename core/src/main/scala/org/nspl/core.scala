package org.nspl

case class Bounds(
    x: Double,
    y: Double,
    w: Double,
    h: Double,
    anchor: Option[Point] = None
) {
  def maxX = x + w
  def maxY = y + h
  def centerX = x + w * 0.5
  def centerY = y + h * 0.5
  def contains(p: Point) =
    p.x >= x && p.x <= x + w &&
      p.y >= y && p.y <= y + h

}

sealed trait Cap
case object CapButt extends Cap
case object CapSquare extends Cap
case object CapRound extends Cap

case class StrokeConf(width: RelFontSize, cap: Cap = CapButt) {
  def value(implicit fc: FontConfiguration) = Stroke(width.value, cap)
}
case class Stroke(width: Double, cap: Cap = CapButt)

case class Point(x: Double, y: Double) {
  def translate(dx: Double, dy: Double) = Point(x + dx, y + dy)
  def transform(tx: AffineTransform) = tx.transform(this)
  def transform(tx: Bounds => AffineTransform) =
    tx(Bounds(x, y, 0, 0)).transform(this)
  def distance(p: Point) = {
    val a = math.abs(x - p.x)
    val b = math.abs(y - p.y)
    math.sqrt(a * a + b * b)
  }
}

trait RenderingContext

trait Renderer[E, R <: RenderingContext] {
  def render(r: R, e: E): Unit
}

/* Basic unit of the scene graph.*/
trait Renderable[K] { self: K =>
  def transform(v: Bounds => AffineTransform): K
  def bounds: Bounds

  def translate(x: Double, y: Double) =
    transform(_ => AffineTransform.translate(x, y))
  def scale(x: Double, y: Double) = transform(_ => AffineTransform.scale(x, y))
  def rotate(rad: Double, x: Double, y: Double) =
    transform(_ => AffineTransform.rotate(rad, x, y))
  def rotate(rad: Double) = transform(_ => AffineTransform.rotate(rad))
  def reflectOrigin = transform(_ => AffineTransform.reflectOrigin)
  def reflectY = transform(_ => AffineTransform.reflectY)
  def reflectX = transform(_ => AffineTransform.reflectX)
  def reflectXCenter = transform(b => AffineTransform.reflectXCenter(b))
  def reflectYCenter = transform(b => AffineTransform.reflectYCenter(b))
  def rotateCenter(rad: Double) =
    transform(b => AffineTransform.rotateCenter(rad)(b))

}

/* Layouts tranform the bounding box of their members. */
trait Layout {
  def apply[F: FC](s: Seq[Bounds]): Seq[Bounds]
}

case class RelFontSize(v: Double) extends AnyVal {
  def *(t: Double) = RelFontSize(v * t)
  def value(implicit fc: FontConfiguration) = v * fc.font.size
  def factor = v
}
case class BaseFontSize(v: Int) extends AnyVal

trait Identifier
case object EmptyIdentifier extends Identifier
case class PlotAreaIdentifier(id: String, bounds: Option[Bounds])
    extends Identifier
