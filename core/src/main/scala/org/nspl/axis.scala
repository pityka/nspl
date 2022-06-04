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
      }
    else
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
      }
    else
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
    tickFormatter: Seq[Double] => Seq[String] = defaultTickFormatter,
    forceMajorTickOnMin: Boolean = false,
    forceMajorTickOnMax: Boolean = false
)(implicit fc: FontConfiguration) {

  def renderable(
      axis: Axis,
      noTickLabel: Boolean,
      disableTicksAt: List[Double]
  ): (List[Double], List[Double], AxisElem) = {

    import axis._

    val horizontal = axis.horizontal

    val tickSpace1 = tickSpace.getOrElse {
      (axis.max - axis.min) / (numTicks)
    }

    val numTicks1 =
      if (tickSpace.isEmpty) numTicks else (axis.max - axis.min) / tickSpace1

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
      val majorTicksExp =
        axis.min +: (lmaj1.map(i => math.pow(10d, i)) :+ axis.max)
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
      else {
        val addMin =
          if (forceMajorTickOnMin && !majorTicks1.contains(axis.min))
            List(axis.min)
          else Nil
        val addMax =
          if (forceMajorTickOnMax && !majorTicks1.contains(axis.max))
            List(axis.max)
          else Nil
        (majorTicks1 ++ addMin ++ addMax).iterator
          .filterNot(x => customTicks.map(_._1).contains(x))
          .filterNot(x => disableTicksAt.contains(x))
          .filter(w => w <= axis.max && w >= axis.min)
          .toList
          .distinct
      }

    def makeTick(world: Double, text: String, availableSpace: Double) = {
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
                TextBox(
                  text,
                  Point(view, 0.0),
                  fontSize = fontSize,
                  width =
                    if (labelRotation == 0.0) Some(availableSpace) else None
                ),
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

    val minorTicks =
      if (numTicks1 == 0 || numMinorTicksFactor <= 0) Nil
      else
        minorTicks1.iterator
          .filter(w => w <= axis.max && w >= axis.min)
          .filterNot(x => customTicks.map(_._1).contains(x))
          .filterNot(majorTicks.contains)
          .toList
          .distinct

    val majorTickLabels = tickFormatter(majorTicks)

    val majorTickElems = sequence({
      val allMajorTicks =
        ((majorTicks zip majorTickLabels) ++ extra).sortBy(_._1)
      val allMajorTicksWithAvailableSpace =
        (List(scala.util.Left(axis.min)) ++ allMajorTicks.map(
          scala.util.Right(_)
        ) ++ List(scala.util.Left(axis.max))).sliding(3).toList.flatMap {
          group =>
            if (group.find(_.isRight).isEmpty) Nil
            else {
              val leftWorld = group(0).fold(identity, _._1)
              val centerWorld = group(1).toOption.get._1
              val rightWorld = group(2).fold(identity, _._1)
              val (world, tick) = group(1).toOption.get
              val leftV = axis.worldToView(leftWorld)
              val centerV = axis.worldToView(centerWorld)
              val rightV = axis.worldToView(rightWorld)
              val availableSpace = (rightV - leftV) * 0.5
              List((world, tick, availableSpace))
            }
        }
      allMajorTicksWithAvailableSpace.map {
        case (world, text, availableSpace) =>
          makeTick(world, if (noTickLabel) "" else text, availableSpace)
      }
    })

    val minorTickElems = sequence(minorTicks.map(makeMinorTick))

    (
      majorTicks,
      customTicks.map(_._1).toList,
      group(line, majorTickElems, minorTickElems, FreeLayout)
    )

  }
}
