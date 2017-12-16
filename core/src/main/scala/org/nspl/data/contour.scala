package org.nspl.data

object contour {
  def apply(
      min1: Double,
      max1: Double,
      min2: Double,
      max2: Double,
      n: Int,
      levels: Int
  )(f: (Double, Double) => Double) = {
    val w1 = (max1 - min1) / n
    val w2 = (max2 - min2) / n
    val cached = 0 to n flatMap { j =>
      0 to n map { i =>
        val x = min1 + i * w1
        val y = min2 + j * w2
        (x, y) -> f(x, y)
      }
    } toMap

    val zs = {
      val zmin = cached.map(_._2).min
      val zmax = cached.map(_._2).max
      0 to levels map { i =>
        zmin + i * (zmax - zmin) / levels
      }
    }

    def neigbours(i: Int, j: Int) =
      ((i + 1, j) :: (i, j + 1) :: Nil).filter(x =>
        x._1 >= 0 && x._2 >= 0 && x._1 <= n && x._2 <= n)

    def segment(
        x1: Double,
        y1: Double,
        v1: Double,
        x2: Double,
        y2: Double,
        v2: Double,
        z: Double
    ) = {
      if ((z <= v1 && z <= v2) || (z >= v1 && z >= v2)) None
      else {

        val t = (v1 - z) / (v1 - v2)
        val x3 = x1 * (1 - t) + x2 * t
        val y3 = y1 * (1 - t) + y2 * t
        Some(x3 -> y3)
      }
    }

    def segmentNeighbours(
        i1: Int,
        j1: Int,
        i2: Int,
        j2: Int
    ) =
      (
        if (j1 == j2)
          List(
            (
              i1,
              j1 + 1,
              i2,
              j2 + 1
            ),
            (i1, j1, i1, j1 + 1),
            (
              i2,
              j1 + 1,
              i2,
              j1
            )
          )
        else if (i1 == i2)
          List(
            (
              i1 + 1,
              j1,
              i2 + 1,
              j2
            ),
            (i1, j1, i1 + 1, j1),
            (
              i2 + 1,
              j2,
              i2,
              j2
            )
          )
        else throw new RuntimeException("")
      ).filter(x =>
        x._1 >= 0 && x._2 >= 0 && x._3 >= 0 && x._4 >= 0 &&
          x._1 <= n && x._2 <= n && x._3 <= n && x._4 <= n)

    zs map { z =>
      val segments = 0 to n flatMap { j =>
        0 to n flatMap { i =>
          val x = min1 + i * w1
          val y = min2 + j * w2
          val v = cached(x -> y)
          neigbours(i, j) flatMap {
            case (n, m) =>
              val x2 = min1 + n * w1
              val y2 = min2 + m * w2
              val v2 = cached(x2 -> y2)
              val intersection = segment(x, y, v, x2, y2, v2, z)
              if (intersection.isDefined) {
                val intersection2 = segmentNeighbours(i, j, n, m)
                  .map {
                    case (i, j, n, m) =>
                      val x = min1 + i * w1
                      val y = min2 + j * w2
                      val v = cached(x -> y)
                      val x2 = min1 + n * w1
                      val y2 = min2 + m * w2
                      val v2 = cached(x2 -> y2)
                      segment(x, y, v, x2, y2, v2, z)
                  }
                  .filter(_.isDefined)
                  .map(_.get)
                  .headOption
                if (intersection2.isDefined)
                  (intersection.get -> intersection2.get) :: Nil
                else Nil
              } else Nil

          }
        }
      }
      z -> segments
    }
  }
}
