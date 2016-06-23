package org.nspl

trait Shapes {

  val shapeList = Vector(
    Ellipse(-5, -5, 10, 10),
    Rectangle(-5, -5, 10, 10), {

      SimplePath(List(Point(-100, -100), Point(-100, 100), Point((-100 + math.sqrt(3) * 100).toInt, 0)))
        .transform(_ => AffineTransform.scale(0.08, 0.08))
    },
    Shape.rectangle(-5, -5, 10, 10).transform(_ => AffineTransform.rotate(0.785398163)), {
      val t = 1.0f
      val l = 5.0f
      val SQRT2 = math.pow(2.0, 0.5)
      SimplePath(
        List(
          Point(-l - t, -l + t),
          Point(-l + t, -l - t),
          Point(0d, -t * SQRT2),
          Point(l - t, -l - t),
          Point(l + t, -l + t),
          Point(t * SQRT2, 0.0),
          Point(l + t, l - t),
          Point(l - t, l + t),
          Point(0.0, t * SQRT2),
          Point(-l + t, l + t),
          Point(-l - t, l - t),
          Point(-t * SQRT2, 0.0),
          Point(-l - t, -l + t)
        )
      )
    }, {
      val t = 1.0f
      val l = 5.0f
      val SQRT2 = math.pow(2.0, 0.5)
      SimplePath(
        List(
          Point(-l - t, -l + t),
          Point(-l + t, -l - t),
          Point(0d, -t * SQRT2),
          Point(l - t, -l - t),
          Point(l + t, -l + t),
          Point(t * SQRT2, 0.0),
          Point(l + t, l - t),
          Point(l - t, l + t),
          Point(0.0, t * SQRT2),
          Point(-l + t, l + t),
          Point(-l - t, l - t),
          Point(-t * SQRT2, 0.0),
          Point(-l - t, -l + t)
        )
      ).transform(_ => AffineTransform.rotate(0.785398163))
    },
    Rectangle(-2, -5, 4, 10),
    Rectangle(-5, -2, 10, 4)
  )

  def shapePick(idx: Int): Shape = shapeList(idx % shapeList.size)

}
