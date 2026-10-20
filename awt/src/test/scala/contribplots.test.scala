package org.nspl

import org.nspl.awtrenderer._

class ContribPlotsSpec extends munit.FunSuite {

  test("scattercontour renders to a non-empty png") {
    val data = (0 until 200).map(i => (math.sin(i * 0.3), math.cos(i * 0.17))).toVector
    val bytes = pngToByteArray(scattercontour(data)(par).build, 300)
    assert(bytes.length > 0)
  }

  test("histogramplot renders multiple series to a non-empty png") {
    val a = (0 until 100).map(i => i.toDouble)
    val b = (40 until 160).map(i => i.toDouble)
    @scala.annotation.nowarn
    val bytes = pngToByteArray(histogramplot(Seq("a" -> a, "b" -> b))(par).build, 300)
    assert(bytes.length > 0)
  }
}
