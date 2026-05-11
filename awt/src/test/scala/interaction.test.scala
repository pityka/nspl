package org.nspl

import org.nspl.awtrenderer._

class InteractionSpec extends munit.FunSuite {

  /** Build a square xyplot covering the world rectangle (0..10, 0..10). */
  private def squarePlot(): (XYPlotArea, Build[XYPlotArea]) = {
    val data = dataSourceFromRows(
      Seq((0d, 0d), (5d, 5d), (10d, 10d))
    )
    val build = xyplotareaBuild(
      List(data -> List(point(noIdentifier = true))),
      AxisSettings(LinearAxisFactory),
      AxisSettings(LinearAxisFactory),
      xlim = Some(0d -> 10d),
      ylim = Some(0d -> 10d),
      // disable padding margins so xMin/xMax = 0/10 exactly
      xAxisMargin = 0d,
      yAxisMargin = 0d
    )
    (build.build, build)
  }

  test("Selection zooms to the world rectangle under the selection box") {
    val (initial, build) = squarePlot()
    assertEqualsDouble(initial.xMin, 0d, 0d)
    assertEqualsDouble(initial.xMax, 10d, 0d)
    assertEqualsDouble(initial.yMin, 0d, 0d)
    assertEqualsDouble(initial.yMax, 10d, 0d)

    val plotAreaId = initial.frameElem.identifier match {
      case p: PlotAreaIdentifier => p
      case other => fail(s"frame elem had identifier $other")
    }
    // Pretend the plot area was rendered in a 100x100 screen rectangle.
    val withBounds = plotAreaId.copy(bounds = Some(Bounds(0, 0, 100, 100)))

    val sel = Selection(Point(25, 25), Point(75, 75), withBounds)
    val next = build((Some(initial), sel))

    assertEqualsDouble(next.xMin, 2.5, 1e-9)
    assertEqualsDouble(next.xMax, 7.5, 1e-9)
    assertEqualsDouble(next.yMin, 2.5, 1e-9)
    assertEqualsDouble(next.yMax, 7.5, 1e-9)
  }

  test("Selection corners may be specified in either order") {
    val (initial, build) = squarePlot()
    val plotAreaId = initial.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .copy(bounds = Some(Bounds(0, 0, 100, 100)))
    val sel = Selection(Point(75, 75), Point(25, 25), plotAreaId)
    val next = build((Some(initial), sel))
    assertEqualsDouble(next.xMin, 2.5, 1e-9)
    assertEqualsDouble(next.xMax, 7.5, 1e-9)
    assertEqualsDouble(next.yMin, 2.5, 1e-9)
    assertEqualsDouble(next.yMax, 7.5, 1e-9)
  }

  test("Selection on a different plot area is ignored") {
    val (initial, build) = squarePlot()
    val otherId = PlotAreaIdentifier(new PlotId, Some(Bounds(0, 0, 100, 100)))
    val sel = Selection(Point(25, 25), Point(75, 75), otherId)
    val next = build((Some(initial), sel))
    // unchanged
    assertEqualsDouble(next.xMin, initial.xMin, 0d)
    assertEqualsDouble(next.xMax, initial.xMax, 0d)
    assertEqualsDouble(next.yMin, initial.yMin, 0d)
    assertEqualsDouble(next.yMax, initial.yMax, 0d)
  }

  test("Degenerate (single-point) Selection is rejected") {
    val (initial, build) = squarePlot()
    val plotAreaId = initial.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .copy(bounds = Some(Bounds(0, 0, 100, 100)))
    val sel = Selection(Point(50, 50), Point(50, 50), plotAreaId)
    val next = build((Some(initial), sel))
    assertEqualsDouble(next.xMin, initial.xMin, 0d)
    assertEqualsDouble(next.xMax, initial.xMax, 0d)
  }

  test("Scroll event zooms (negative delta in, positive delta out)") {
    val (initial, build) = squarePlot()
    val plotAreaId = initial.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .copy(bounds = Some(Bounds(0, 0, 100, 100)))
    val before = initial.xMax - initial.xMin

    val zin = build((Some(initial), Scroll(-1d, Point(50, 50), plotAreaId)))
    val zout = build((Some(initial), Scroll(1d, Point(50, 50), plotAreaId)))

    assert(
      zin.xMax - zin.xMin < before,
      s"expected zoom-in: range=${zin.xMax - zin.xMin} >= $before"
    )
    assert(
      zout.xMax - zout.xMin > before,
      s"expected zoom-out: range=${zout.xMax - zout.xMin} <= $before"
    )
  }

  test("ShapeElem.withIdentifier round-trips") {
    val id = DataRowIdx(0, 0, 7)
    val s = ShapeElem(Shape.circle(1d)).withIdentifier(id)
    assertEquals(s.identifier, id: Identifier)
  }

  test("TextBox.withIdentifier and apply identifier round-trip") {
    val id = TextBoxIdentifier("legend", 3)
    val t1 = TextBox("hi").withIdentifier(id)
    val t2 = TextBox("hi", identifier = id)
    assertEquals(t1.identifier, id: Identifier)
    assertEquals(t2.identifier, id: Identifier)
  }

  test("DataRowIdx and TextBoxIdentifier are distinct types") {
    val data: Identifier = DataRowIdx(1, 2, 3)
    val text: Identifier = TextBoxIdentifier("a", 0)
    // Compile-time test: pattern matching disambiguates.
    val dataKind = data match {
      case _: DataRowIdx        => "data"
      case _: TextBoxIdentifier => "text"
      case _                    => "other"
    }
    val textKind = text match {
      case _: DataRowIdx        => "data"
      case _: TextBoxIdentifier => "text"
      case _                    => "other"
    }
    assertEquals(dataKind, "data")
    assertEquals(textKind, "text")
  }
}
