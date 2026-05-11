package org.nspl

class EventFusionHelperSpec extends munit.FunSuite {

  private def plotArea(canFuse: Boolean = true): PlotAreaIdentifier =
    PlotAreaIdentifier(new PlotId, Some(Bounds(0, 0, 100, 100)), canFuse)

  test("non-fusable events are appended verbatim") {
    val s = new EventFusionHelper
    val a = plotArea()
    val e1 = Drag(Point(0, 0), Point(10, 10), a)
    val e2 = Drag(Point(10, 10), Point(20, 20), a)
    s.add(e1, fusable = false)
    s.add(e2, fusable = false)
    assertEquals(s.get, Vector(e1, e2))
  }

  test("consecutive incremental drags fuse into the cumulative drag") {
    val s = new EventFusionHelper
    val a = plotArea()
    s.add(Drag(Point(0, 0), Point(10, 10), a), fusable = true)
    s.add(Drag(Point(10, 10), Point(25, 30), a), fusable = true)
    assertEquals(s.size, 1)
    assertEquals(s.get.head, Drag(Point(0, 0), Point(25, 30), a))
  }

  test("drags with the same start collapse to the latest endpoint") {
    val s = new EventFusionHelper
    val a = plotArea()
    s.add(Drag(Point(0, 0), Point(5, 5), a), fusable = true)
    s.add(Drag(Point(0, 0), Point(50, 50), a), fusable = true)
    assertEquals(s.size, 1)
    assertEquals(s.get.head, Drag(Point(0, 0), Point(50, 50), a))
  }

  test("drags on different plot areas do not fuse") {
    val s = new EventFusionHelper
    val a = plotArea()
    val b = plotArea()
    s.add(Drag(Point(0, 0), Point(10, 10), a), fusable = true)
    s.add(Drag(Point(10, 10), Point(20, 20), b), fusable = true)
    assertEquals(s.size, 2)
  }

  test("selection grows from a fixed start corner") {
    val s = new EventFusionHelper
    val a = plotArea()
    s.add(Selection(Point(0, 0), Point(10, 10), a), fusable = true)
    s.add(Selection(Point(0, 0), Point(30, 40), a), fusable = true)
    assertEquals(s.size, 1)
    assertEquals(s.get.head, Selection(Point(0, 0), Point(30, 40), a))
  }

  test("selections that don't share a start corner stack up") {
    val s = new EventFusionHelper
    val a = plotArea()
    s.add(Selection(Point(0, 0), Point(10, 10), a), fusable = true)
    s.add(Selection(Point(5, 5), Point(15, 15), a), fusable = true)
    assertEquals(s.size, 2)
  }

  test("scroll events are not fused with each other") {
    val s = new EventFusionHelper
    val a = plotArea()
    s.add(Scroll(1.0, Point(10, 10), a), fusable = true)
    s.add(Scroll(1.0, Point(10, 10), a), fusable = true)
    assertEquals(s.size, 2)
  }

  test("a drag followed by a scroll keeps both") {
    val s = new EventFusionHelper
    val a = plotArea()
    s.add(Drag(Point(0, 0), Point(5, 5), a), fusable = true)
    s.add(Scroll(1.0, Point(5, 5), a), fusable = true)
    assertEquals(s.size, 2)
  }

  test("canFuseEvents=false on the identifier doesn't itself prevent fusion") {
    // The store doesn't read the flag — it is the *caller's* responsibility
    // to pass `fusable = id.canFuseEvents` from the canvas event loop. This
    // pins that contract down so a future refactor can't drift.
    val s = new EventFusionHelper
    val a = plotArea(canFuse = false)
    s.add(Drag(Point(0, 0), Point(5, 5), a), fusable = false)
    s.add(Drag(Point(5, 5), Point(10, 10), a), fusable = false)
    assertEquals(s.size, 2)
  }

  test("clear empties the store") {
    val s = new EventFusionHelper
    val a = plotArea()
    s.add(Drag(Point(0, 0), Point(5, 5), a), fusable = true)
    s.clear()
    assertEquals(s.size, 0)
  }
}
