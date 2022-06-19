package org.nspl.data

import scala.collection.mutable.ArrayBuffer
import org.nspl._

private[nspl] trait DataAdaptors extends DataTuples {

  def dataSourceFrom1DSeq(vec: Seq[Double]): DataTable =
    new DataTable(vec.toArray, 1)

  implicit def zipped2(
      vec1: (Seq[Double], Seq[Double])
  ): DataSourceWithQuantiles =
    dataSourceFromRows(vec1._1 zip vec1._2)

  implicit def zipped3(
      vec1: (Seq[Double], Seq[Double], Seq[Double])
  ): DataSourceWithQuantiles =
    dataSourceFromRows(
      vec1._1 zip vec1._2 zip vec1._3 map (x => (x._1._1, x._1._2, x._2))
    )

  implicit def histogram(h: HistogramData): DataSource = {
    dataSourceFromRows(h.toScatter)
  }

  def rasterFromStream(
      s1: Iterator[Double],
      numCols: Int,
      numRows: Int,
      minmax: MinMax
  ): DataSource =
    new DataSource {

      def iterator = s1.zipWithIndex.map { case (v, i) =>
        productsToRow(((i % numCols).toDouble, (i / numCols).toDouble, v))
      }
      def dimension = 3
      def columnMinMax(i: Int) = i match {
        case 0 => Some(MinMaxImpl(0, numCols - 1d))
        case 1 => Some(MinMaxImpl(0, numRows - 1d))
        case 2 => Some(minmax)
      }
    }

  def rasterFromSeq(
      s1: Seq[Double],
      numCols: Int,
      numRows: Int
  ): DataSource =
    new DataSource {

      def iterator = s1.zipWithIndex.iterator.map { case (v, i) =>
        productsToRow(((i % numCols).toDouble, (i / numCols).toDouble, v))
      }
      def dimension = 3

      private val min = if (s1.isEmpty) Double.NaN else s1.min
      private val max = if (s1.isEmpty) Double.NaN else s1.max
      def columnMinMax(i: Int) = if (s1.isEmpty) None
      else
        i match {
          case 0 => Some(MinMaxImpl(0, numCols - 1d))
          case 1 => Some(MinMaxImpl(0, numRows - 1d))
          case 2 => Some(MinMaxImpl(min, max))
        }
    }

  def dataSource[T](s1: Iterator[T], minmax: IndexedSeq[MinMax])(implicit
      f: T => Row
  ): DataSource =
    new DataSource {

      def iterator = s1.map(f)
      def dimension = minmax.size
      def columnMinMax(i: Int) = Some(minmax(i))
    }

  /** Need to iterate twice on the data: once for the bounds to get the axis
    * right, once for the plot
    */
  def dataSource[T](s1: Iterator[T], s2: Iterator[T])(implicit
      f: T => Row
  ): DataSource =
    new DataSource {

      private var max = scala.collection.mutable.ArrayBuffer[Double]()
      private var min = scala.collection.mutable.ArrayBuffer[Double]()
      private var columncount: Option[Int] = None

      s2.map(f).foreach { row =>
        if (columncount.isEmpty) {
          columncount = Some(row.dimension)
          max = ArrayBuffer.fill(columncount.get)(Double.MinValue)
          min = ArrayBuffer.fill(columncount.get)(Double.MaxValue)
        }

        row.allColumns.zipWithIndex.foreach { case (v, i) =>
          if (max(i) < v) {
            max(i) = v
          }
          if (min(i) > v) {
            min(i) = v
          }
        }
      }

      def iterator = s1.map(f)
      def dimension = columncount.get
      def columnMinMax(i: Int) = Some(MinMaxImpl(min(i), max(i)))
    }

  implicit def dataSourceFromRows[T](
      s: Seq[T]
  )(implicit f: T => Row): DataSourceWithQuantiles =
    new DataSourceWithQuantiles {
      def iterator = s.iterator.map(f)
      def dimension = s.headOption.map(v => f(v).dimension).getOrElse(0)

      private var max = scala.collection.mutable.ArrayBuffer[Double]()
      private var min = scala.collection.mutable.ArrayBuffer[Double]()
      private var columncount: Option[Int] = None

      s.map(f).foreach { row =>
        if (columncount.isEmpty) {
          columncount = Some(row.dimension)
          max = ArrayBuffer.fill(columncount.get)(Double.MinValue)
          min = ArrayBuffer.fill(columncount.get)(Double.MaxValue)
        }

        row.allColumns.zipWithIndex.foreach { case (v, i) =>
          if (max(i) < v) {
            max(i) = v
          }
          if (min(i) > v) {
            min(i) = v
          }
        }
      }

      def columnMinMax(i: Int) =
        if (s.isEmpty) None
        else Some(MinMaxImpl(min(i), max(i)))
      def quantilesOfColumn(i: Int, qs: Vector[Double]) = {
        val v = s.map(v => f(v).apply(i))
        percentile(v, qs).toVector
      }
      override def toString = s"DataSourceFrom($s)"
    }

  implicit def indexed(s: Seq[Double]): DataSourceWithQuantiles =
    new DataSourceWithQuantiles {
      def iterator =
        s.iterator.zipWithIndex.map(x => productsToRow(x._2.toDouble -> x._1))
      def dimension = 2

      private val min = if (s.isEmpty) Double.NaN else s.min
      private val max = if (s.isEmpty) Double.NaN else s.max
      def columnMinMax(i: Int) = if (s.isEmpty) None
      else
        i match {
          case 0 => Some(MinMaxImpl(0, s.size - 1d))
          case 1 => Some(MinMaxImpl(min, max))
          case _ => None
        }
      def quantilesOfColumn(i: Int, qs: Vector[Double]) = {
        assert(i == 1 || i == 0)
        val v =
          if (i == 1) s.sorted
          else (0 until s.size).map(_.toDouble)
        percentile(v, qs).toVector
      }
    }

  def boxplotData(s: DataSourceWithQuantiles): DataSource = {
    val list = (0 until s.dimension).flatMap { i =>
      val minmax = s.columnMinMax(i)
      if (minmax.isEmpty) None
      else {
        val quantiles = s.quantilesOfColumn(i, Vector(0.25, 0.5, 0.75))

        Some(
          VectorRow(
            Vector(
              i.toDouble,
              quantiles(1),
              quantiles(0),
              quantiles(2),
              minmax.get.min,
              minmax.get.max
            ),
            s.columnNames(i)
          )
        )
      }
    }
    list
  }

  def boxplotData[T: Ordering](s: Seq[(T, Double)]): DataSource = {
    val list = s.groupBy(_._1).toSeq.sortBy(_._1).zipWithIndex.flatMap {
      case ((label, data), i) =>
        val s = dataSourceFrom1DSeq(data.map(_._2))
        s.columnMinMax(0).map { minmax =>
          val quantiles = s.quantilesOfColumn(0, Vector(0.25, 0.5, 0.75))

          VectorRow(
            Vector(
              i.toDouble,
              quantiles(1),
              quantiles(0),
              quantiles(2),
              minmax.min,
              minmax.max
            ),
            label.toString
          )
        }
    }
    list
  }

  def boxplotData(
      s: DataSourceWithQuantiles,
      x: Vector[Double],
      colors: Vector[Double],
      labels: Vector[String]
  ): DataSource = {
    val list = (0 until s.dimension).flatMap { i =>
      s.columnMinMax(i).map { minmax =>
        val quantiles = s.quantilesOfColumn(i, Vector(0.25, 0.5, 0.75))

        VectorRow(
          Vector(
            x(i),
            quantiles(1),
            quantiles(0),
            quantiles(2),
            minmax.min,
            minmax.max,
            x(i) + 1,
            colors(i)
          ),
          labels(i)
        )
      }
    }
    list
  }

  def boxplotData(
      dim1: Seq[Double],
      dim2: Seq[Double],
      quantiles: Seq[Double],
      colors: Vector[Double]
  ): DataSource = {
    val boundaries =
      HistogramData.makeBoundariesFromPercentiles(dim2, quantiles)
    val binned = HistogramData.bin(dim1, dim2, boundaries)
    binned.zipWithIndex.filterNot(_._1._3.isEmpty).map {
      case ((a, b, group), i) =>
        val min = group.min
        val max = group.max
        val quantiles = percentile(group, Vector(0.25, 0.5, 0.75))
        VectorRow(
          Vector(
            (a + b) * 0.5,
            quantiles(1),
            quantiles(0),
            quantiles(2),
            min,
            max,
            b - a,
            colors(i)
          ),
          ""
        )
    }
  }

  def density(
      data: IndexedSeq[Double],
      bandwidth: Double = 0.0,
      n: Int = 50
  ): DataSourceWithQuantiles = {
    val min = data.min
    val max = data.max
    val w = (max - min) / n
    val h =
      if (bandwidth <= 0.0)
        1.06 * math.sqrt(sampleVariance(data)) * math.pow(n.toDouble, -0.2)
      else bandwidth

    0 to n map { i =>
      val x = min + i * w
      x -> KDE.univariate(data.toArray, x, h)
    }
  }

  def density2d[F: FC](
      data: IndexedSeq[(Double, Double)],
      bandwidth: Double = 0.1,
      n: Int = 50,
      levels: Int = 10,
      stroke: StrokeConf = StrokeConf(lineWidth),
      color: Colormap = HeatMapColors(0, 1)
  ) = {
    val min1 = data.map(_._1).min
    val max1 = data.map(_._1).max
    val min2 = data.map(_._2).min
    val max2 = data.map(_._2).max
    val w1 = (max1 - min1) / n
    val w2 = (max2 - min2) / n

    linesegments(
      contour(
        min1,
        max1,
        min2,
        max2,
        n,
        levels
      )((x, y) => KDE.density2d(data, (x, y), bandwidth)),
      stroke = stroke,
      color = color
    )

  }

  def densityMatrix(
      data: IndexedSeq[(Double, Double)],
      bandwidth: Double = 0.1,
      xlim: Option[(Double, Double)] = None,
      ylim: Option[(Double, Double)] = None,
      n: Int = 50
  ) = {

    val min1 = xlim.map(_._1) getOrElse data.map(_._1).min
    val max1 = xlim.map(_._2) getOrElse data.map(_._1).max
    val min2 = ylim.map(_._1) getOrElse data.map(_._2).min
    val max2 = ylim.map(_._2) getOrElse data.map(_._2).max
    val w1 = linspace(min1, max1, n)
    val w2 = linspace(min2, max2, n)

    new DataMatrix(
      w2.flatMap { y =>
        w1.map { x =>
          KDE.density2d(data, (x, y), bandwidth)
        }
      }.toArray,
      n,
      n
    )

  }

  def linesegments[F: FC](
      data: Seq[(Double, Seq[((Double, Double), (Double, Double))])],
      stroke: StrokeConf = StrokeConf(lineWidth),
      color: Colormap = HeatMapColors(0, 1)
  ) = {
    val datasource: DataSourceWithQuantiles = data.flatMap { case (z, pairs) =>
      pairs.map { case ((x1, y1), (x2, y2)) =>
        VectorRow(Vector(x1, y1, x2, y2, z), "")
      }
    }
    val zmin = data.map(_._1).min
    val zmax = data.map(_._1).max
    datasource -> org.nspl
      .lineSegment(stroke = stroke, color = color.withRange(zmin, zmax))
  }

  def hexbin[F: FC](
      data: Iterator[(Double, Double)],
      xlim: (Double, Double),
      ylim: (Double, Double),
      size: Double,
      color: Colormap,
      logCounts: Boolean
  ) = {
    val binning = HexBin.apply(data, xlim, ylim, size, logCounts)
    val renderer = point(
      colorCol = 2,
      color = color,
      size = size * math.sqrt(3),
      shapes = Vector(Shape.hexagon(1)),
      pointSizeIsInDataSpaceUnits = true,
      keepPointShapeAspectRatio = true
    )
    (binning, renderer)
  }

  def hexbin[F: FC](
      data: Seq[(Double, Double)],
      size: Double,
      color: Colormap,
      logCounts: Boolean = false
  ) = {
    val xmin = data.map(_._1).min + size * math.sqrt(3) * 0.5
    val xmax = data.map(_._1).max - size * math.sqrt(3) * 0.5
    val ymin = data.map(_._2).min + size
    val ymax = data.map(_._2).max - size
    val binning =
      HexBin.apply(data.iterator, (xmin, xmax), (ymin, ymax), size, logCounts)
    val renderer = point(
      colorCol = 2,
      color = color,
      size = size * math.sqrt(3),
      shapes = Vector(Shape.hexagon(1)),
      pointSizeIsInDataSpaceUnits = true,
      keepPointShapeAspectRatio = true
    )
    (binning, renderer)
  }

  def hexbin[F: FC](
      data: DataSource,
      size: Double,
      color: Colormap,
      logCounts: Boolean
  ) = {
    val xMinMax = data.columnMinMax(0).get
    val yMinMax = data.columnMinMax(1).get
    val binning = HexBin.apply(
      data.iterator.map { row =>
        row(0) -> row(1)
      },
      (
        xMinMax.min + size * math.sqrt(3) * 0.5,
        xMinMax.max - size * math.sqrt(3) * 0.5
      ),
      (yMinMax.min + size, yMinMax.max - size),
      size,
      logCounts
    )
    val renderer = point(
      colorCol = 2,
      color = color,
      size = size * math.sqrt(3),
      shapes = Vector(Shape.hexagon(1)),
      pointSizeIsInDataSpaceUnits = true,
      keepPointShapeAspectRatio = true
    )
    (binning, renderer)
  }
}
