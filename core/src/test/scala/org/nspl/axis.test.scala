package org.nspl

class AxisSpec extends munit.FunSuite {

  private val eps = 1e-9

  private def roundTrip(axis: Axis, values: Seq[Double]): Unit =
    values.foreach { v =>
      val rt = axis.viewToWorld(axis.worldToView(v))
      assert(
        math.abs(rt - v) < eps * math.max(1.0, math.abs(v)),
        s"worldToView ∘ viewToWorld($v) = $rt; horizontal=${axis.horizontal}, log=${axis.log}"
      )
    }

  test("LinearAxisFactory horizontal: viewToWorld inverts worldToView") {
    val ax = LinearAxisFactory.make(-10d, 30d, 200d, horizontal = true)
    roundTrip(ax, Seq(-10d, -5d, 0d, 7.5, 12.345, 30d))
  }

  test("LinearAxisFactory vertical: viewToWorld inverts worldToView") {
    val ax = LinearAxisFactory.make(0d, 100d, 400d, horizontal = false)
    roundTrip(ax, Seq(0d, 25d, 50d, 99d, 100d))
  }

  test("Linear: view endpoints match width endpoints") {
    val ax = LinearAxisFactory.make(0d, 50d, 100d, horizontal = true)
    assertEqualsDouble(ax.worldToView(0d), 0d, eps)
    assertEqualsDouble(ax.worldToView(50d), 100d, eps)
    assertEqualsDouble(ax.viewToWorld(0d), 0d, eps)
    assertEqualsDouble(ax.viewToWorld(100d), 50d, eps)
  }

  test("Linear vertical: y axis is inverted in view space") {
    val ax = LinearAxisFactory.make(0d, 100d, 200d, horizontal = false)
    // For a vertical axis y=min is drawn at the *bottom* (view = width),
    // y=max at the top (view = 0).
    assertEqualsDouble(ax.worldToView(0d), 200d, eps)
    assertEqualsDouble(ax.worldToView(100d), 0d, eps)
  }

  test("Log10AxisFactory: viewToWorld inverts worldToView") {
    val ax = Log10AxisFactory.make(1d, 1000d, 300d, horizontal = true)
    roundTrip(ax, Seq(1d, 5d, 10d, 100d, 999d))
    assert(ax.log)
    assert(!ax.isLog2)
  }

  test("Log10: midpoint of view = geometric midpoint of world range") {
    val ax = Log10AxisFactory.make(1d, 10000d, 100d, horizontal = true)
    // log10(1)=0, log10(10000)=4, midpoint of log = 2 → world = 100.
    assertEqualsDouble(ax.viewToWorld(50d), 100d, 1e-6)
  }

  test("Log2AxisFactory: viewToWorld inverts worldToView") {
    val ax = Log2AxisFactory.make(1d, 64d, 200d, horizontal = true)
    roundTrip(ax, Seq(1d, 2d, 4d, 8d, 16d, 32d, 64d))
    assert(ax.log)
    assert(ax.isLog2)
  }

  test("Log2 vertical: viewToWorld inverts worldToView") {
    val ax = Log2AxisFactory.make(2d, 32d, 150d, horizontal = false)
    roundTrip(ax, Seq(2d, 4d, 8d, 16d, 32d))
  }

  test("Log10 throws on non-positive worldToView input") {
    val ax = Log10AxisFactory.make(1d, 100d, 100d, horizontal = true)
    intercept[RuntimeException](ax.worldToView(0d))
    intercept[RuntimeException](ax.worldToView(-1d))
  }
}
