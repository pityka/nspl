package org.nspl

import org.nspl.data.HistogramData

class HistogramSpec extends munit.FunSuite {

  test("density normalizes total area to 1") {
    val hist = HistogramData(Seq(1d, 1d, 2d, 2d, 3d, 3d, 4d), 1.0)
    val d = hist.density
    val totalArea = d.bins.iterator.map {
      case ((x1, x2, _), y) => (x2 - x1) * y
    }.sum
    assertEqualsDouble(totalArea, 1.0, 1e-9)
  }

  test("density on an already-normalized histogram is idempotent in shape") {
    val hist = HistogramData(Seq(1d, 2d, 3d, 4d, 5d), 1.0)
    val d1 = hist.density
    val d2 = d1.density
    // Same bin centers, same (relative) heights
    val ks1 = d1.bins.keys.toSet
    val ks2 = d2.bins.keys.toSet
    assertEquals(ks1, ks2)
    ks1.foreach { k =>
      assertEqualsDouble(d1.bins(k), d2.bins(k), 1e-9)
    }
  }

  test("density of an empty histogram is a no-op (avoids divide-by-zero)") {
    val empty = HistogramData(Seq.empty[Double], 1.0)
    val d = empty.density
    assertEquals(d.bins, empty.bins)
  }

  test("toScatter drops zero-height bins") {
    // bin (0,1) → 0 occurrences, bin (1,2) → 2, bin (2,3) → 0, bin (3,4) → 1
    val hist = HistogramData(Seq(1d, 1d, 3d), 1.0)
    val scatter = hist.toScatter
    // Every emitted row should have a positive top (= ystart + height > 0).
    scatter.foreach(row => assert(row._2 > 0, s"unexpected empty bin: $row"))
    // We expect exactly the non-empty bins.
    assertEquals(scatter.size, hist.bins.count(_._2 > 0))
  }

  test("toScatter is sorted by x ascending") {
    val hist = HistogramData(Seq(5d, 1d, 3d, 1d, 4d), 1.0)
    val xs = hist.toScatter.map(_._1)
    assertEquals(xs.toList, xs.sorted.toList)
  }

  test("relative reweights by total count") {
    val hist = HistogramData(Seq(1d, 1d, 2d, 2d, 3d), 1.0)
    val rel = hist.relative
    // Sum of all bin values should be ≤ 1 (since each value is now count/n)
    val total = rel.bins.values.sum
    assert(total >= 0.99 && total <= 1.01, s"sum was $total")
  }
}
