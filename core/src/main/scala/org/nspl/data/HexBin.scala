package org.nspl.data

object HexBin {

  /** Hexagonal binning
    *
    * Algorithm:
    * https://cran.r-project.org/web/packages/hexbin/vignettes/hexagon_binning.pdf
    * page10 and: https://www.redblobgames.com/grids/hexagons/
    *
    * Creates two Cartesian coordinate systems both with base (width,height) but
    * with an offset origin Each integer valued coordinate (i,j) in those two
    * coordinate systems are hexagon centers. Quickly find the 2 possible
    * closest center of a point by taking advantage of the Cartesian coordinate
    * system Tests the 2 candidates
    */
  def apply(
      data: Iterator[(Double, Double)],
      xlim: (Double, Double),
      ylim: (Double, Double),
      size: Double,
      log: Boolean
  ) = {
    // base
    val width = size * math.sqrt(3d)
    val height = size * 2 * 1.5

    // lattice 1
    val xOrigin1 = xlim._1
    val yOrigin1 = ylim._1

    // lattice 2
    val xOrigin2 = xlim._1 + width * 0.5
    val yOrigin2 = ylim._1 + height * 0.5

    val bins = scala.collection.mutable.AnyRefMap[(Double, Double), Long]()

    data.foreach { case (x, y) =>
      // lattice 1 coordinates
      val x1 = (x - xOrigin1) / width
      val y1 = (y - yOrigin1) / height

      // lattice 2 coordinates
      val x2 = (x - xOrigin2) / width
      val y2 = (y - yOrigin2) / height

      def transformBack1(pair: (Double, Double)) =
        (pair._1 * width + xOrigin1, pair._2 * height + yOrigin1)

      def transformBack2(pair: (Double, Double)) =
        (pair._1 * width + xOrigin2, pair._2 * height + yOrigin2)

      val candidates = List(
        transformBack1(x1.floor, y1.floor),
        transformBack1(x1.floor, y1.ceil),
        transformBack1(x1.ceil, y1.floor),
        transformBack1(x1.ceil, y1.ceil),
        transformBack2(x2.floor, y2.floor),
        transformBack2(x2.floor, y2.ceil),
        transformBack2(x2.ceil, y2.floor),
        transformBack2(x2.ceil, y2.ceil)
      )

      val selectedCenter = candidates.minBy { case (centerX, centerY) =>
        (centerX - x) * (centerX - x) + (centerY - y) * (centerY - y)
      }

      bins.get(selectedCenter) match {
        case None        => bins.update(selectedCenter, 1)
        case Some(count) => bins.update(selectedCenter, count + 1)
      }
    }

    bins.toSeq.map { case ((x, y), count) =>
      (x, y, if (log) math.log10(count.toDouble) else count.toDouble)
    }

  }

}
