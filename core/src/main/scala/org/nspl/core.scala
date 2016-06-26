package org.nspl

case class Bounds(x: Double, y: Double, w: Double, h: Double) {
  def maxX = x + w
  def maxY = y + h
  def centerX = x + w * 0.5
  def centerY = y + h * 0.5
}

case class Stroke(width: Double)

case class Point(x: Double, y: Double) {
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
  def rotateCenter(rad: Double) = transform(b => AffineTransform.rotateCenter(rad)(b))

}

trait Layout {
  def apply(s: Seq[Bounds]): Seq[Bounds]
}

case class RelFontSize(v: Double) extends AnyVal
case class BaseFontSize(v: Double) extends AnyVal
