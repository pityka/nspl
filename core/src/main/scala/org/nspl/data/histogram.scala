package org.nspl.data

case class HistogramData(
    bins: Map[(Double, Double, Double), Double],
    minX: Double,
    maxX: Double,
    maxY: Double,
    n: Int,
    lastBinUpperBound: Double
) {

  def relative =
    HistogramData(
      bins.map(x => (x._1._1, x._1._2, x._1._3 / n) -> x._2 / n),
      minX,
      maxX,
      maxY / n,
      1,
      lastBinUpperBound
    )

  def filterBinsMinValue(m: Double) = this.copy(bins = bins.filter(_._2 >= m))

  def /(that: HistogramData): HistogramData =
    if ((this.bins.keys.toSet & that.bins.keys.toSet).size == 0)
      throw new IllegalArgumentException(
        "Should have the same binning. " + this.toString + " / " +
          that.toString
      )
    else {
      val newbins = bins
        .map { bin1 =>
          val thisX = bin1._1._1
          val thisXEnd = bin1._1._2
          val thisY = bin1._1._3
          val thisValue = bin1._2
          val thatValue = that.bins.get(bin1._1)
          thatValue.map { tv =>
            (thisX, thisXEnd, thisY / tv) -> thisValue / tv
          }
        }
        .filter(_.isDefined)
        .map(_.get)
        .toMap
      HistogramData(
        newbins,
        minX,
        maxX,
        newbins.values.max,
        1,
        lastBinUpperBound
      )
    }

  def toScatter =
    bins.toSeq.map { case ((xstart, xend, ystart), height) =>
      ((xstart), (ystart + height), 0d, xend - xstart, ystart)
    } sortBy (_._1)
}
object HistogramData {

  /** Merge maps with key collision
    * @param fun
    *   Handles key collision
    */
  private def addMaps[K, V](a: Map[K, V], b: Map[K, V])(
      fun: (V, V) => V
  ): Map[K, V] = {
    a ++ b.map { case (key, bval) =>
      val aval = a.get(key)
      val cval = aval match {
        case None    => bval
        case Some(a) => fun((a), (bval))
      }
      (key, cval)
    }
  }

  def makeBoundaries(
      step: Double,
      min: Double,
      max: Double
  ): Seq[(Double, Double)] = {
    assert(max >= min, "max < min")
    assert(step > 0, "step < 0")
    val c = ((max - min) / step).toInt
    0 until c map { i =>
      (min + step * i) -> (min + step * (i + 1))
    } toSeq
  }

  def makeBoundariesFromPercentiles(
      data: Seq[Double],
      qs: Seq[Double]
  ): Seq[(Double, Double)] = {
    val ps = percentile(data, qs)
    (data.min +: ps.dropRight(1)) zip ps
  }

  def bin(
      data: Seq[Double],
      binning: Seq[Double],
      boundaries: Seq[(Double, Double)]
  ): Seq[(Double, Double, Seq[Double])] = {
    val zp = data zip binning
    boundaries.map { b =>
      (b._1, b._2, zp.filter(d => b._1 <= d._2 && b._2 > d._2).map(_._1))
    }
  }

  def apply(
      data: Seq[Double],
      binBoundaries: Seq[(Double, Double)]
  ): HistogramData = {

    val min = binBoundaries.minBy(_._1)._1
    val max = binBoundaries.maxBy(_._2)._2
    val data2 = data
      .filter(x => !x.isNaN && !x.isInfinite)
      .filter(z => z <= max && z >= min)

    val displayMax = max // binBoundaries.maxBy(_._2)._2

    val bins: Map[(Double, Double, Double), Double] = binBoundaries.map {
      case (x, y) =>
        (x, y, 0.0) -> (if (y == max)
                          data2.filter(z => z <= y && z >= x).size.toDouble
                        else data2.filter(z => z < y && z >= x).size.toDouble)
    }.toMap

    val datasize = data2.size

    assert(data2.size.toDouble == bins.map(_._2).sum)

    HistogramData(bins, min, displayMax, bins.values.max, datasize, displayMax)
  }

  def apply(
      data: Seq[Double],
      step: Double,
      min: Double,
      max: Double
  ): HistogramData = {

    HistogramData(data, makeBoundaries(step, min, max))
  }

  def apply(data: Seq[Double], step: Double): HistogramData = {

    val data2 = data.filter(x => !x.isNaN && !x.isInfinite)
    if (data2.isEmpty) {
      HistogramData(
        bins = Map[(Double, Double, Double), Double](),
        minX = 1.0,
        maxX = 1.0,
        maxY = 0.0,
        n = 0,
        lastBinUpperBound = 1.0
      )
    } else {
      val min = data2.min
      val max = data2.max

      HistogramData(data2, step, min, max)
    }
  }
  def apply(data: Seq[Double], breaks: Int): HistogramData = {

    val step = getStep(data, breaks)

    apply(data, step)
  }

  def fromStratifiedData(
      data: Seq[Seq[Double]],
      step: Double,
      relativePerBin: Boolean
  ): Seq[HistogramData] = {
    val maxX = data.flatten.max
    val minX = data.flatten.min

    val data2 = data.map(_.filter(x => !x.isNaN && !x.isInfinite))

    val min = data2.flatten.min
    val max = data2.flatten.max

    val displayMax = max //(max - min) + step + min

    val baseBins = ((0 to (((max - min) / step).toInt + 1)) map { (idx: Int) =>
      ((min + idx * step), 0.0) -> 0.0
    }).toMap

    val bins: Seq[Map[(Double, Double), Double]] = data2
      .map { stratum =>
        baseBins ++ stratum.zipWithIndex
          .map { case (value, idx) =>
            val i = ((value - min) / step).toInt
            val bin = i * step + min
            bin -> value
          }
          .groupBy(_._1)
          .map(x => (x._1, 0.0) -> x._2.map(_._2).size.toDouble)
          .toMap
      }
      .foldLeft(Seq[Map[(Double, Double), Double]]()) {
        case (acc, binOfStratum) =>
          if (acc.isEmpty) Seq(binOfStratum)
          else {
            val head = acc.head
            val updatedBin = binOfStratum.map { case ((x, y), dens) =>
              (
                x,
                head.filter(_._1._1 == x).headOption.map(_._2).getOrElse(0.0) +
                  head
                    .filter(_._1._1 == x)
                    .headOption
                    .map(_._1._2)
                    .getOrElse(0.0)
              ) -> dens
            }.toMap
            updatedBin +: acc
          }
      }

    if (relativePerBin) {
      val perBinMaxY: Map[Double, Double] = bins
        .map(_.groupBy(_._1._1).map(x => x._1 -> x._2.map(_._2).sum))
        .reduce((x, y) => addMaps(x, y)(_ + _))

      val newbins = bins.map(
        _.map(x =>
          (x._1._1, x._1._2 / perBinMaxY(x._1._1)) -> x._2 / perBinMaxY(
            x._1._1
          )
        )
      )
      newbins.map { bin =>
        HistogramData(
          bin.map { case (x, v) => (x._1, x._1 + step, x._2) -> v },
          minX,
          maxX,
          1,
          1,
          displayMax
        )
      }
    } else {

      val maxY =
        if (bins.isEmpty) 0
        else
          bins
            .map(_.map(x => x._1._1 -> x._2))
            .reduce((x, y) => addMaps(x, y)(_ + _))
            .values
            .max

      bins.map { bin =>
        HistogramData(
          bin.map { case (x, v) => (x._1, x._1 + step, x._2) -> v },
          minX,
          maxX,
          maxY,
          data.map(_.size).sum,
          displayMax
        )
      }
    }
  }

  def getStep(data: Seq[Double], breaks: Int) = {
    val data2 = data.filter(x => !x.isNaN && !x.isInfinite)
    val min = data2.min
    val max = data2.max
    if (max == min) 1.0
    else {
      val breaks2: Int =
        if (breaks <= 0) (math.pow(2d * data.size, 0.5).toInt + 1) else breaks
      val ret = (max - min) / breaks2
      assert(
        ret > 0,
        "step < 0 " + ret + " " + breaks2 + " " + data.toString +
          " breaks: " + breaks
      )
      ret
    }
  }
}
