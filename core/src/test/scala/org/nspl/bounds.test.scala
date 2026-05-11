package org.nspl

class BoundsSpec extends munit.FunSuite {

  test("fromPoints accepts any corner ordering") {
    val cases = List(
      (Point(0d, 0d), Point(10d, 5d)),
      (Point(10d, 5d), Point(0d, 0d)),
      (Point(10d, 0d), Point(0d, 5d)),
      (Point(0d, 5d), Point(10d, 0d))
    )
    cases.foreach { case (p1, p2) =>
      val b = Bounds.fromPoints(p1, p2)
      assertEqualsDouble(b.x, 0d, 0d)
      assertEqualsDouble(b.y, 0d, 0d)
      assertEqualsDouble(b.w, 10d, 0d)
      assertEqualsDouble(b.h, 5d, 0d)
    }
  }

  test("fromPoints of a single point produces a zero-area rectangle") {
    val b = Bounds.fromPoints(Point(3d, 7d), Point(3d, 7d))
    assertEqualsDouble(b.w, 0d, 0d)
    assertEqualsDouble(b.h, 0d, 0d)
    assert(b.contains(Point(3d, 7d)))
  }

  test("contains is inclusive of all four edges") {
    val b = Bounds(0d, 0d, 10d, 4d)
    assert(b.contains(Point(0d, 0d)))
    assert(b.contains(Point(10d, 0d)))
    assert(b.contains(Point(0d, 4d)))
    assert(b.contains(Point(10d, 4d)))
    assert(b.contains(Point(5d, 2d)))
  }

  test("contains rejects points outside") {
    val b = Bounds(0d, 0d, 10d, 4d)
    assert(!b.contains(Point(-0.1, 2d)))
    assert(!b.contains(Point(10.1, 2d)))
    assert(!b.contains(Point(5d, -0.1)))
    assert(!b.contains(Point(5d, 4.1)))
  }

  test("centerX / centerY / maxX / maxY") {
    val b = Bounds(2d, 4d, 10d, 6d)
    assertEqualsDouble(b.centerX, 7d, 0d)
    assertEqualsDouble(b.centerY, 7d, 0d)
    assertEqualsDouble(b.maxX, 12d, 0d)
    assertEqualsDouble(b.maxY, 10d, 0d)
  }
}
