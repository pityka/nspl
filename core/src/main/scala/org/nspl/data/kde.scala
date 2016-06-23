package org.nspl.data

object KDE {
  def normPDF(x: Double, u: Double, v: Double) =
    1.0 / math.sqrt(2 * v * math.Pi) * math.exp((-1) * (x - u) * (x - u) / (2 * v))

  def univariate(d: IndexedSeq[Double], x: Double, bandwidth: Double) = {
    val n = d.size
    1d / (n * bandwidth) * d.foldLeft(0.0)((s, xi) => normPDF((x - xi) / bandwidth, 0, 1) + s)
  }

  def density2d(d: IndexedSeq[(Double, Double)], x: (Double, Double), bw1: Double, bw2: Double) = {
    // def norm2(x: (Double, Double), y: (Double, Double)) = x._1 * y._1 + x._2 * y._2
    val n = 1.0 / d.size
    n * d.foldLeft(0.0) { (s, xi) =>
      (1 / bw1 * normPDF(x._1 - xi._1 / bw1, 0, 1) *
        1 / bw2 * normPDF(x._2 - xi._2 / bw2, 0, 1)) + s
    }
  }
}
