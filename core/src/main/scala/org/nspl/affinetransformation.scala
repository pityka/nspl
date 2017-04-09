package org.nspl

/**
  * 2D Affine Transformation Matrix in row major order
  */
case class AffineTransform(m0: Double,
                           m1: Double,
                           m2: Double,
                           m3: Double,
                           m4: Double,
                           m5: Double) {

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
    val topLeft = transform(Point(b.x, b.y))
    val topRight = transform(Point(b.x + b.w, b.y))
    val bottomRight = transform(Point(b.x + b.w, b.y + b.h))
    val bottomLeft = transform(Point(b.x, b.y + b.h))

    val pointsX = Array(topLeft.x, topRight.x, bottomRight.x, bottomLeft.x)
    val pointsY = Array(topLeft.y, topRight.y, bottomRight.y, bottomLeft.y)

    var nx = Double.MaxValue
    var nx2 = Double.MinValue
    var ny = Double.MaxValue
    var ny2 = Double.MinValue

    var i = 0
    while (i < 4) {
      val x = pointsX(i)
      val y = pointsY(i)

      if (x < nx) { nx = x }
      if (x > nx2) { nx2 = x }
      if (y < ny) { ny = y }
      if (y > ny2) { ny2 = y }

      i += 1
    }

    val width = math.abs(nx2 - nx)
    val height = math.abs(ny2 - ny)
    Bounds(nx, ny, width, height)
  }

  def concat(tx: AffineTransform) = AffineTransform(
    m0 * tx.m0 + m1 * tx.m3,
    m0 * tx.m1 + m1 * tx.m4,
    m0 * tx.m2 + m1 * tx.m5 + m2,
    m3 * tx.m0 + m4 * tx.m3,
    m3 * tx.m1 + m4 * tx.m4,
    m3 * tx.m2 + m4 * tx.m5 + m5
  )
}

object AffineTransform {

  val reflectXCenter = (b: Bounds) =>
    reflectX.concat(translate(0, b.h * (-1) - b.y))

  val reflectYCenter = (b: Bounds) =>
    reflectY.concat(translate(b.w * (-1) - b.x, 0))

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
      x - math.cos(rad) * x - math.sin(rad) * y,
      -1 * math.sin(rad),
      math.cos(rad),
      y + math.sin(rad) * x - math.cos(rad) * y
    )
}
