package org.nspl

/** A rectangle for bounding boxes
  *
  * @param anchor
  *   an optional point termed the anchor. Certain layouts align to the anchor
  *   rather than to the edges of the bounding box
  */
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

/** Line cap style */
sealed trait Cap
object Cap {
  case object Butt extends Cap
  case object Square extends Cap
  case object Round extends Cap
}

/** Font dependent Stroke
  *
  * Width and dash are expressed in terms of a relative font size
  */
case class StrokeConf(
    width: RelFontSize,
    cap: Cap = Cap.Butt,
    dash: Seq[RelFontSize] = Seq.empty
) {
  def value(implicit fc: FontConfiguration) =
    Stroke(width.value, cap, dash.map(_.value))
}

/** Font independent Stroke
  *
  * Width and dash are expressed in terms of doubles
  */
case class Stroke(
    width: Double,
    cap: Cap = Cap.Butt,
    dash: Seq[Double] = Seq.empty
)

/** 2D point */
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

/** Abstract rendering context
 *
 * Provides methods to manipulate a state machine of transformation states
 */
trait RenderingContext[A <: RenderingContext[A]] { self: A =>
  type LocalTx
  def concatTransform(tx: AffineTransform): Unit
  def setTransform(tx: LocalTx): Unit
  def getTransform: LocalTx
  def localToScala(tx: LocalTx): AffineTransform
  def getAffineTransform: AffineTransform = localToScala(getTransform)
  def withTransform[T](tx: AffineTransform)(f: => T) = {
    val current = getTransform
    concatTransform(tx)
    val r = f
    setTransform(current)
    r
  }
  def render[K <: Renderable[K]](k: K)(implicit r: Renderer[K, A]) =
    r.render(self, k)
}

/** A Renderer provides a way to render a type into a RenderingContext
  *
  * Concrete RenderingContext implementations need a Shape and TextBox renderer.
  */
trait Renderer[E, R <: RenderingContext[R]] {
  def render(r: R, e: E): Unit
}

/** Basic unit of the scene graph.*/
trait Renderable[K] { self: K =>
  def transform(v: (Bounds, AffineTransform) => AffineTransform): K
  def transform(v: AffineTransform): K
  def bounds: Bounds

  def translate(x: Double, y: Double) =
    transform((_, old) => old.translate(x, y))
  def scale(x: Double, y: Double) = transform((_, old) => old.scale(x, y))
  def rotate(rad: Double, x: Double, y: Double) =
    transform((_, old) => old.rotate(rad, x, y))
  def rotate(rad: Double) = transform((_, old) => old.rotate(rad))
  def rotateCenter(rad: Double) =
    transform((b, old) => old.rotate(rad, b.centerX, b.centerY))

}

/** Layouts tranform the bounding box of their members. */
trait Layout {
  def apply[F: FC](s: Seq[Bounds]): Seq[Bounds]
}

/** Relative font size
  *
  * A relative font size of 1 represents the horizontal space taken by one
  * letter
  */
class RelFontSize(val factor: Double) extends AnyVal {
  def *(t: Double) = new RelFontSize(factor * t)
  def value(implicit fc: FontConfiguration) = factor * fc.font.size
}

/** A raw reference used for reference based equality tests
 *
 * Used to identify certain parts of a composit plot
 */
class PlotId

/** Semantic information about parts of a plot */
trait Identifier
case object EmptyIdentifier extends Identifier

/** Final rendered bounds (if available) and identifier of a plot area
  */
case class PlotAreaIdentifier(id: PlotId, bounds: Option[Bounds])
    extends Identifier
