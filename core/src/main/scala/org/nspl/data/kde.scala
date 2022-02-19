package org.nspl.data

object KDE {
  def normPDF(x: Double) =
    1.0 / math.sqrt(2 * math.Pi) * math.exp(
      -0.5 * x * x
    )

  def univariate(d: Array[Double], center: Double, bandwidth: Double) = {
    val n = d.size
    var s = 0d
    var i = 0
    while (i < n) {
      s += normPDF((center - d(i)) / bandwidth)
      i += 1
    }
    s / (n * bandwidth)

  }

  def density2d(
      d: IndexedSeq[(Double, Double)],
      x: (Double, Double),
      bw: Double
  ) = {
    // def norm2(x: (Double, Double), y: (Double, Double)) = x._1 * y._1 + x._2 * y._2
    val n = 1.0 / (d.size * bw * bw)
    val rbw = 1d / bw
    n * d.foldLeft(0.0) { (s, xi) =>
      val sqdist =
        (x._1 - xi._1) * (x._1 - xi._1) + (x._2 - xi._2) * (x._2 - xi._2)
      math.exp(-0.5 * sqdist * rbw) + s
    }
  }
}
