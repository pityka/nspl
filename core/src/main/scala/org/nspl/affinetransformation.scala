package org.nspl

/** 2D Affine Transformation Matrix in row major order
  */
case class AffineTransform(
    m0: Double,
    m1: Double,
    m2: Double,
    m3: Double,
    m4: Double,
    m5: Double
) {

  def inverse = {
    val a = m0
    val b = m1
    val c = m3
    val d = m4
    val idet = 1.0 / (a * d - b * c)
    val ia = idet * d
    val ib = -1 * idet * b
    val ic = -1 * idet * c
    val id = idet * a
    val idx = -1 * (ia * m2 + ib * m5)
    val idy = -1 * (ic * m2 + id * m5)
    AffineTransform(ia, ib, idx, ic, id, idy)
  }

  def transform(stroke: Stroke): Stroke = {
    val oldWidth = stroke.width
    val fx = {
      val tp1 = this.transform(0d, 0d)
      val tp2 = this.transform(0d, 1d)
      tp1.distance(tp2)
    }
    Stroke(oldWidth * fx, stroke.cap, stroke.dash.map(_ * fx))
  }

  def transform(x: Double, y: Double): Point = {
    val bx = m2
    val by = m5
    val a00 = m0
    val a01 = m1
    val a10 = m3
    val a11 = m4
    val nx = bx + a00 * x + a01 * y
    val ny = by + a10 * x + a11 * y
    Point(nx, ny)
  }

  // 0  1  2  3  4  5  6  7  8
  // 00 01 02 10 11 12 20 21 22
  def transform(p: Point): Point = {
    val bx = m2
    val by = m5
    val a00 = m0
    val a01 = m1
    val a10 = m3
    val a11 = m4

    val nx = bx + a00 * p.x + a01 * p.y
    val ny = by + a10 * p.x + a11 * p.y

    Point(nx, ny)
  }
  def transform(b: Bounds): Bounds = {
    val topLeft = transform(b.x, b.y)
    val topRight = transform(b.x + b.w, b.y)
    val bottomRight = transform(b.x + b.w, b.y + b.h)
    val bottomLeft = transform(b.x, b.y + b.h)
    val transformedAnchor = b.anchor.map(p => transform(p))

    val nx = math.min(
      topLeft.x,
      math.min(topRight.x, math.min(bottomRight.x, bottomLeft.x))
    )
    val nx2 = math.max(
      topLeft.x,
      math.max(topRight.x, math.max(bottomRight.x, bottomLeft.x))
    )
    val ny = math.min(
      topLeft.y,
      math.min(topRight.y, math.min(bottomRight.y, bottomLeft.y))
    )
    val ny2 = math.max(
      topLeft.y,
      math.max(topRight.y, math.max(bottomRight.y, bottomLeft.y))
    )

    val width = math.abs(nx2 - nx)
    val height = math.abs(ny2 - ny)
    Bounds(nx, ny, width, height, transformedAnchor)
  }

  /** returns (this mm tx) in terms of transformations returns a transformation
    * which first applies tx then applies this
    */
  def applyBefore(tx: AffineTransform) =
    if (tx == AffineTransform.identity) this
    else
      AffineTransform(
        m0 * tx.m0 + m1 * tx.m3,
        m0 * tx.m1 + m1 * tx.m4,
        m0 * tx.m2 + m1 * tx.m5 + m2,
        m3 * tx.m0 + m4 * tx.m3,
        m3 * tx.m1 + m4 * tx.m4,
        m3 * tx.m2 + m4 * tx.m5 + m5
      )

  /** returns (tx mm this) in terms of transformations returns a transformation
    * which first applies this then applies tx
    */
  def andThen(tx: AffineTransform) =
    if (tx == AffineTransform.identity) this
    else
      tx.applyBefore(this)

  /** returns a transformation which applies this then rotates around (x,y) with
    * rad
    */
  def rotate(rad: Double, x: Double, y: Double) = {
    val tm0 = math.cos(rad)
    val tm1 = math.sin(rad)
    val tm2 = x - tm0 * x - tm1 * y
    val tm3 = -1 * tm1
    val tm4 = tm0
    val tm5 = y + tm1 * x - tm0 * y
    AffineTransform(
      tm0 * m0 + tm1 * m3,
      tm0 * m1 + tm1 * m4,
      tm0 * m2 + tm1 * m5 + tm2,
      tm3 * m0 + tm4 * m3,
      tm3 * m1 + tm4 * m4,
      tm3 * m2 + tm4 * m5 + tm5
    )
  }

  /** returns a transformation which applies this then rotates around (0,0) with
    * rad
    */
  def rotate(rad: Double) = {
    val tm0 = math.cos(rad)
    val tm1 = math.sin(rad)
    val tm3 = -1 * tm1
    val tm4 = tm0
    AffineTransform(
      tm0 * m0 + tm1 * m3,
      tm0 * m1 + tm1 * m4,
      tm0 * m2 + tm1 * m5,
      tm3 * m0 + tm4 * m3,
      tm3 * m1 + tm4 * m4,
      tm3 * m2 + tm4 * m5
    )
  }

  /** Returns a transformation which applies this then scales then translates
   */
  def scaleThenTranslate(tx: Double, ty: Double, sx: Double, sy: Double) =
    AffineTransform(
      m0 * sx,
      m1 * sx,
      m2 * sx + tx,
      m3 * sy,
      m4 * sy,
      m5 * sy + ty
    )

  /** Returns a tranformation which applies this then translates */
  def translate(x: Double, y: Double) = AffineTransform(
    m0,
    m1,
    m2 + x,
    m3,
    m4,
    m5 + y
  )

  /** Returns a tranformation which applies this then scales */
  def scale(x: Double, y: Double) =
    AffineTransform(
      m0 * x,
      m1 * x,
      m2 * x,
      m3 * y,
      m4 * y,
      m5 * y
    )
}

object AffineTransform {

  def rotateCenter(rad: Double) =
    (b: Bounds) => rotate(rad, b.centerX, b.centerY)

  val identity = AffineTransform(1d, 0d, 0d, 0d, 1d, 0d)

  val reflectOrigin = AffineTransform(-1d, 0d, 0d, 0d, -1d, 0d)

  val reflectX = AffineTransform(1d, 0d, 0d, 0d, -1d, 0d)

  val reflectY = AffineTransform(-1d, 0d, 0d, 0d, 1d, 0d)

  def translate(x: Double, y: Double) =
    AffineTransform(1d, 0d, x, 0d, 1d, y)

  def scale(x: Double, y: Double) =
    AffineTransform(x, 0d, 0d, 0d, y, 0)

  /** Returns a transformation which first scales then translates */
  def scaleThenTranslate(tx: Double, ty: Double, sx: Double, sy: Double) =
    AffineTransform(sx, 0d, tx, 0d, sy, ty)

  def translateThenScale(tx: Double, ty: Double, sx: Double, sy: Double) =
    AffineTransform(sx, 0d, sx * tx, 0d, sy, sy * ty)

  def rotate(rad: Double) =
    AffineTransform(
      math.cos(rad),
      math.sin(rad),
      0,
      -1 * math.sin(rad),
      math.cos(rad),
      0
    )

  def rotate(rad: Double, x: Double, y: Double) =
    AffineTransform(
      math.cos(rad),
      math.sin(rad),
      -math.cos(rad) * x - math.sin(rad) * y + x,
      -1 * math.sin(rad),
      math.cos(rad),
      -math.cos(rad) * y + math.sin(rad) * x + y
    )
}
