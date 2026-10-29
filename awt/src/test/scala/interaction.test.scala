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
    val withBounds = plotAreaId.withBounds(Some(Bounds(0, 0, 100, 100)))

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
      .withBounds(Some(Bounds(0, 0, 100, 100)))
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
      .withBounds(Some(Bounds(0, 0, 100, 100)))
    val sel = Selection(Point(50, 50), Point(50, 50), plotAreaId)
    val next = build((Some(initial), sel))
    assertEqualsDouble(next.xMin, initial.xMin, 0d)
    assertEqualsDouble(next.xMax, initial.xMax, 0d)
  }

  test("Scroll event zooms (negative delta in, positive delta out)") {
    val (initial, build) = squarePlot()
    val plotAreaId = initial.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .withBounds(Some(Bounds(0, 0, 100, 100)))
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

  test("mouseToWorld maps canvas-space mouse to world (linear axes)") {
    val (initial, _) = squarePlot()
    val baseId = initial.frameElem.identifier.asInstanceOf[PlotAreaIdentifier]
    // Simulate the canvas backend filling in the canvas-space bounds.
    val id = baseId.withBounds(Some(Bounds(0, 0, 100, 100)))

    // Center: (50, 50) → world (5, 5)
    assertEquals(id.mouseToWorld(Point(50, 50)), Some(Point(5d, 5d)))
    // Top-left in canvas is yMax in world: (0, 0) → (0, 10)
    assertEquals(id.mouseToWorld(Point(0, 0)), Some(Point(0d, 10d)))
    // Bottom-right in canvas is yMin in world: (100, 100) → (10, 0)
    assertEquals(id.mouseToWorld(Point(100, 100)), Some(Point(10d, 0d)))
    // Off-center: (25, 75) → (2.5, 2.5)
    assertEquals(id.mouseToWorld(Point(25, 75)), Some(Point(2.5d, 2.5d)))
  }

  test("mouseToWorld respects a non-zero canvas bounds origin") {
    val (initial, _) = squarePlot()
    val baseId = initial.frameElem.identifier.asInstanceOf[PlotAreaIdentifier]
    // Plot area is rendered at canvas (200, 100) with size 100x100.
    val id = baseId.withBounds(Some(Bounds(200, 100, 100, 100)))

    assertEquals(id.mouseToWorld(Point(250, 150)), Some(Point(5d, 5d)))
    assertEquals(id.mouseToWorld(Point(200, 100)), Some(Point(0d, 10d)))
    assertEquals(id.mouseToWorld(Point(300, 200)), Some(Point(10d, 0d)))
  }

  test("mouseToWorld returns None when bounds are missing") {
    val (initial, _) = squarePlot()
    val id = initial.frameElem.identifier.asInstanceOf[PlotAreaIdentifier]
    assertEquals(id.bounds, None)
    assertEquals(id.mouseToWorld(Point(50, 50)), None)
  }

  test("mouseToWorld returns None when axes are missing") {
    val id = PlotAreaIdentifier(
      new PlotId,
      bounds = Some(Bounds(0, 0, 100, 100)),
      viewBounds = Some(Bounds(0, 0, 100, 100))
    )
    assertEquals(id.mouseToWorld(Point(50, 50)), None)
  }

  test("mouseToWorld inverts worldToView on a log y-axis") {
    val data = dataSourceFromRows(Seq((1d, 1d), (10d, 100d)))
    val build = xyplotareaBuild(
      List(data -> List(point(noIdentifier = true))),
      AxisSettings(LinearAxisFactory),
      AxisSettings(Log10AxisFactory),
      xlim = Some(1d -> 10d),
      ylim = Some(1d -> 100d),
      xAxisMargin = 0d,
      yAxisMargin = 0d
    )
    val initial = build.build
    val id = initial.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .withBounds(Some(Bounds(0, 0, 100, 100)))

    // halfway down in canvas-y is log10 midpoint in world-y = 10
    val world = id.mouseToWorld(Point(50, 50)).get
    assertEqualsDouble(world.x, 5.5, 1e-9)
    assertEqualsDouble(world.y, 10d, 1e-9)

    // top of canvas → world y max
    assertEqualsDouble(id.mouseToWorld(Point(50, 0)).get.y, 100d, 1e-9)
    // bottom of canvas → world y min
    assertEqualsDouble(id.mouseToWorld(Point(50, 100)).get.y, 1d, 1e-9)
  }

  test("MouseHover adds a crosshair, MouseLeave removes it") {
    val (initial, build) = squarePlot()
    // The initial plot has no crosshair.
    val baseCrosshairCount =
      initial.elem.m1.m1.m1.m6.members.size
    assertEquals(baseCrosshairCount, 0)

    val plotAreaId = initial.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .withBounds(Some(Bounds(0, 0, 100, 100)))

    // Hover at the center of the canvas → world (5, 5).
    val hovered = build((Some(initial), MouseHover(Point(50, 50), plotAreaId)))
    val hoveredCrosshair = hovered.elem.m1.m1.m1.m6.members
    // Two lines: horizontal + vertical.
    assertEquals(hoveredCrosshair.size, 2)

    // Mouse leaves → crosshair gone.
    val left = build((Some(hovered), MouseLeave(plotAreaId)))
    assertEquals(left.elem.m1.m1.m1.m6.members.size, 0)
  }

  /** Build a square xyplot with a configurable crosshair mode. */
  private def squarePlotWithMode(
      mode: CrosshairMode
  ): (XYPlotArea, Build[XYPlotArea]) = {
    val data = dataSourceFromRows(Seq((0d, 0d), (5d, 5d), (10d, 10d)))
    val build = xyplotareaBuild(
      List(data -> List(point(noIdentifier = true))),
      AxisSettings(LinearAxisFactory),
      AxisSettings(LinearAxisFactory),
      xlim = Some(0d -> 10d),
      ylim = Some(0d -> 10d),
      xAxisMargin = 0d,
      yAxisMargin = 0d,
      crosshairMode = mode
    )
    (build.build, build)
  }

  test("CrosshairMode.Horizontal renders only the horizontal line on hover") {
    val (initial, build) = squarePlotWithMode(CrosshairMode.Horizontal)
    val plotAreaId = initial.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .withBounds(Some(Bounds(0, 0, 100, 100)))
    val hovered = build((Some(initial), MouseHover(Point(50, 50), plotAreaId)))
    assertEquals(hovered.elem.m1.m1.m1.m6.members.size, 1)
  }

  test("CrosshairMode.Vertical renders only the vertical line on hover") {
    val (initial, build) = squarePlotWithMode(CrosshairMode.Vertical)
    val plotAreaId = initial.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .withBounds(Some(Bounds(0, 0, 100, 100)))
    val hovered = build((Some(initial), MouseHover(Point(50, 50), plotAreaId)))
    assertEquals(hovered.elem.m1.m1.m1.m6.members.size, 1)
  }

  test("CrosshairMode.None suppresses the crosshair on hover") {
    val (initial, build) = squarePlotWithMode(CrosshairMode.None)
    val plotAreaId = initial.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .withBounds(Some(Bounds(0, 0, 100, 100)))
    val hovered = build((Some(initial), MouseHover(Point(50, 50), plotAreaId)))
    assertEquals(hovered.elem.m1.m1.m1.m6.members.size, 0)
  }

  test("CrosshairMode survives zoom (Scroll) rebuilds") {
    val (initial, build) = squarePlotWithMode(CrosshairMode.Horizontal)
    val plotAreaId = initial.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .withBounds(Some(Bounds(0, 0, 100, 100)))
    // Zoom in first, then hover on the resulting plot — the rebuilt plot
    // must still honor the configured mode.
    val zoomed = build((Some(initial), Scroll(-1d, Point(50, 50), plotAreaId)))
    val zoomedId = zoomed.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .withBounds(Some(Bounds(0, 0, 100, 100)))
    val hovered = build((Some(zoomed), MouseHover(Point(50, 50), zoomedId)))
    assertEquals(hovered.elem.m1.m1.m1.m6.members.size, 1)
  }

  test("MouseHover preserves an existing zoom (xlim/ylim)") {
    val (initial, build) = squarePlot()
    val plotAreaId = initial.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .withBounds(Some(Bounds(0, 0, 100, 100)))
    val zoomed = build((Some(initial), Selection(Point(25, 25), Point(75, 75), plotAreaId)))
    assertEqualsDouble(zoomed.xMin, 2.5, 1e-9)
    assertEqualsDouble(zoomed.xMax, 7.5, 1e-9)

    // The zoomed plot has fresh axes; reuse its frame's identifier.
    val zoomedId = zoomed.frameElem.identifier
      .asInstanceOf[PlotAreaIdentifier]
      .withBounds(Some(Bounds(0, 0, 100, 100)))
    val hovered = build((Some(zoomed), MouseHover(Point(50, 50), zoomedId)))
    // Range must be preserved — hover must not reset the zoom.
    assertEqualsDouble(hovered.xMin, 2.5, 1e-9)
    assertEqualsDouble(hovered.xMax, 7.5, 1e-9)
    assertEquals(hovered.elem.m1.m1.m1.m6.members.size, 2)
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
