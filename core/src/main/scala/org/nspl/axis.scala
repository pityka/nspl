package org.nspl

trait Axis {
  def worldToView(v: Double): Double
  def min: Double
  def max: Double
  def width = math.abs(worldToView(max) - worldToView(min))
  def horizontal: Boolean
  def log: Boolean
}

sealed trait AxisFactory {
  def make(min: Double, max: Double, width: Double, horizontal: Boolean): Axis
}

object LinearAxisFactory extends AxisFactory {
  def make(min1: Double, max1: Double, width1: Double, horizontal: Boolean) =
    if (horizontal)
      new Axis {
        val tmp = width1 / (max1 - min1)
        def worldToView(v: Double) = (v - min1) * tmp
        val min = if (min1 == max1) max1 - 1d else min1
        val max = if (min1 == max1) max1 + 1d else max1
        val horizontal = true
        val log = false
      } else
      new Axis {

        val tmp = width1 / (max1 - min1)

        def worldToView(v: Double) =
          width1 - (v - min1) * tmp
        val min = if (min1 == max1) max1 - 1d else min1
        val max = if (min1 == max1) max1 + 1d else max1
        val horizontal = false
        val log = false
      }
}

object Log10AxisFactory extends AxisFactory {
  def make(min1: Double, max1: Double, width1: Double, horizontal: Boolean) = {
    val lMin1 = math.log10(min1)
    val lMax1 = math.log10(max1)
    if (horizontal)
      new Axis {
        def worldToView(v: Double) = {
          if (v <= 0d) throw new RuntimeException("<0")
          (math.log10(v) - lMin1) / (lMax1 - lMin1) * width1
        }
        val min = if (min1 == max1) max1 - 1d else min1
        val max = if (min1 == max1) max1 + 1d else max1
        val horizontal = true
        val log = true
      } else
      new Axis {
        def worldToView(v: Double) = {
          if (v <= 0d) throw new RuntimeException("<0")
          width1 - (math.log10(v) - lMin1) / (lMax1 - lMin1) * width1
        }
        val min = if (min1 == max1) max1 - 1d else min1
        val max = if (min1 == max1) max1 + 1d else max1
        val horizontal = false
        val log = true
      }
  }
}

case class AxisSettings(
    axisFactory: AxisFactory,
    numTicks: Int = 4,
    tickSpace: Option[Double] = None,
    baseTick: Option[Double] = None,
    numMinorTicksFactor: Int = 5,
    tickLength: RelFontSize = .35 fts,
    tickLabelDistance: RelFontSize = 0.5 fts,
    customTicks: Seq[(Double, String)] = Nil,
    labelRotation: Double = 0,
    width: RelFontSize = 20 fts,
    fontSize: RelFontSize = 1 fts,
    tickAlignment: Double = -1.0,
    lineWidth: RelFontSize = lineWidth,
    lineLengthFraction: Double = 1d,
    lineStartFraction: Double = 0.0,
    tickFormatter: Seq[Double] => Seq[String] = defaultTickFormatter
)(implicit fc: FontConfiguration) {

  def renderable(
      axis: Axis,
      disableTicksAt: List[Double] = Nil
  ): (List[Double], List[Double], AxisElem) = {

    import axis._

    val horizontal = axis.horizontal

    val tickSpace1 = tickSpace.getOrElse {
      (axis.max - axis.min) / (numTicks)
    }

    val numTicks1 =
      if (tickSpace.isEmpty) numTicks else (axis.max - axis.min) / tickSpace1

    def makeTick(world: Double, text: String) = {
      val view = worldToView(world)
      if (horizontal)
        group(
          ShapeElem(
            Shape
              .line(
                Point(view, 0d),
                Point(view, tickAlignment * tickLength.value)
              ),
            stroke = Some(Stroke(lineWidth.value))
          ),
          transform(
            transform(
              transform(
                TextBox(text, Point(view, 0.0), fontSize = fontSize),
                (b: Bounds) =>
                  AffineTransform.rotate(labelRotation, b.x, b.centerY)
              ),
              (b: Bounds) =>
                AffineTransform.translate(0, tickLabelDistance.value)
            ),
            (b: Bounds) =>
              AffineTransform
                .translate(if (labelRotation == 0.0) b.w * (-0.5) else 0, 0)
          ),
          FreeLayout
        )
      else
        group(
          ShapeElem(
            Shape.line(
              Point(0d, view),
              Point(-1 * tickAlignment * tickLength.value, view)
            ),
            stroke = Some(Stroke(lineWidth.value))
          ),
          transform(
            transform(
              transform(
                TextBox(text, Point(0.0, view), fontSize = fontSize),
                (b: Bounds) =>
                  AffineTransform.rotate(labelRotation, b.x + b.w, b.centerY)
              ),
              (b: Bounds) =>
                AffineTransform.translate(-1 * tickLabelDistance.value - b.w, 0)
            ),
            (b: Bounds) =>
              AffineTransform
                .translate(0, if (labelRotation == 0.0) b.h * (-0.5) else 0)
          ),
          FreeLayout
        )
    }

    def makeMinorTick(world: Double) = {
      val view = worldToView(world)
      if (horizontal)
        ShapeElem(
          Shape.line(
            Point(view, 0d),
            Point(view, tickLength.value * 0.5 * tickAlignment)
          ),
          stroke = Some(Stroke(lineWidth.value))
        )
      else
        ShapeElem(
          Shape.line(
            Point(0d, view),
            Point(-1 * tickLength.value * 0.5 * tickAlignment, view)
          ),
          stroke = Some(Stroke(lineWidth.value))
        )
    }

    val lineStart = lineStartFraction * axis.width
    val lineEnd = lineStart + axis.width * lineLengthFraction

    val line =
      if (horizontal)
        ShapeElem(
          Shape.line(Point(lineStart, 0d), Point(lineEnd, 0d)),
          stroke = Some(Stroke(lineWidth.value))
        )
      else
        ShapeElem(
          Shape.line(Point(0d, lineStart), Point(0d, lineEnd)),
          stroke = Some(Stroke(lineWidth.value))
        )

    val extra =
      customTicks
        .filter(i => i._1 >= axis.min && i._1 <= axis.max)
        .map(i => makeTick(i._1, i._2))

    val (majorTicks1, minorTicks1) = if (axis.log) {
      val lmaj1 =
        (math
          .log10(axis.min)
          .ceil
          .toInt until math
          .log10(axis.max)
          .toInt)
          .map(_.toDouble)
          .filter { i =>
            val e = math.pow(10d, i)
            e >= axis.min - 1e-3 && e <= axis.max + 1e-3
          }
      val majorTicksExp = axis.min +: (lmaj1.map(i => math.pow(10d, i)) :+ axis.max)
      val minorTicksExp = majorTicksExp
        .sliding(2)
        .flatMap { group =>
          val m1 = group(0)
          val m2 = group(1)
          val space = (m2 - m1) / numMinorTicksFactor
          (0 to numMinorTicksFactor.toInt).map(i => m1 + i * space)
        }
        .filterNot(majorTicksExp.contains)
        .toList
      (majorTicksExp, minorTicksExp)
    } else
      Ticks.heckbert(axis.min, axis.max, numTicks1.toInt, numMinorTicksFactor)

    val majorTicks =
      if (numTicks1 == 0) Nil
      else
        majorTicks1.iterator
          .filterNot(x => customTicks.map(_._1).contains(x))
          .filterNot(x => disableTicksAt.contains(x))
          .filter(w => w <= axis.max && w >= axis.min)
          .toList
          .distinct

    val minorTicks =
      if (numTicks1 == 0 || numMinorTicksFactor <= 0) Nil
      else
        minorTicks1.iterator
          .filterNot(x => customTicks.map(_._1).contains(x))
          .filter(w => w <= axis.max && w >= axis.min)
          .filterNot(majorTicks.contains)
          .toList
          .distinct

    val majorTickLabels = tickFormatter(majorTicks)

    val majorTickElems = sequence(
      (majorTicks zip majorTickLabels)
        .map { case (world, text) => makeTick(world, text) } ++ extra
    )

    val minorTickElems = sequence(minorTicks.map(makeMinorTick))

    (
      majorTicks,
      customTicks.map(_._1).toList,
      group(line, majorTickElems, minorTickElems, FreeLayout)
    )

  }
}
