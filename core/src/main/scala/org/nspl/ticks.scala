package org.nspl

object Ticks {

  /** Modified Heckbert's algorithm
    * Translated and modified from the source of R's labeling package. https://cran.r-project.org/web/packages/labeling/index.html
    */
  def heckbert(
      dataMin: Double,
      dataMax: Double,
      numTicksMajor: Int,
      numTicksMinorFactor: Int
  ): (Seq[Double], Seq[Double]) = {
    def niceNum(x: Double, round: Boolean) = {
      val e = math.floor(math.log10(x))
      val f = x / math.pow(10d, e)
      val nf = if (round) {
        if (f < 1.5) 1
        else if (f < 3) 2
        else if (f < 7) 5
        else 10
      } else {
        if (f <= 1) 1
        else if (f <= 2) 2
        else if (f <= 5) 5
        else 10
      }
      (nf.toDouble * math.pow(10d, e))

    }

    def seq(min: Double, max: Double, by: Double) = {
      val nstep = ((max - min) / by).toInt
      (0 to nstep).map(i => min + i * by)
    }

    val lstep = niceNum((dataMax - dataMin) / (numTicksMajor - 1d), true)
    val lmin = math.floor(dataMin / lstep) * lstep
    val lmax = math.ceil(dataMax / lstep) * lstep
    val majorTicks = seq(lmin, lmax, lstep)
    val minorTicks =
      seq(lmin, lmax, lstep / numTicksMinorFactor)
        .filterNot(l => majorTicks.contains(l))
    (majorTicks.map(_.toDouble), minorTicks.map(_.toDouble))
  }
}
