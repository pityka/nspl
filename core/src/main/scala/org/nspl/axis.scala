package org.nspl

trait Axis {
  def viewToWorld(v: Double): Double
  def worldToView(v: Double): Double
  def min: Double
  def max: Double
  def width = math.abs(worldToView(max) - worldToView(min))
  def horizontal: Boolean
}

trait AxisFactory {
  def make(min: Double, max: Double, width: Double, horizontal: Boolean): Axis
}

object LinearAxisFactory extends AxisFactory {
  def make(min1: Double, max1: Double, width1: Double, horizontal: Boolean) =
    if (horizontal)
      new Axis {
        def viewToWorld(v: Double) = v / width1 * (max1 - min1)
        def worldToView(v: Double) = (v - min1) / (max1 - min1) * width1
        def min = if (min1 == max1) max1 - 1d else min1
        def max = if (min1 == max1) max1 + 1d else max1
        def horizontal = true
      } else
      new Axis {
        def viewToWorld(v: Double) = (width1 - v) / width1 * (max1 - min1)
        def worldToView(v: Double) =
          width1 - (v - min1) / (max1 - min1) * width1
        def min = if (min1 == max1) max1 - 1d else min1
        def max = if (min1 == max1) max1 + 1d else max1
        def horizontal = false
      }
}

// object Log10AxisFactory extends AxisFactory {
//   def make(min1: Double, max1: Double, width1: Double) = new Axis {
//     val min2 = math.log10(min1)
//     val max2 = math.log10(max1)
//     def viewToWorld(v: Double) = {
//       val lin = v / width1 * (max2 - min2)
//       math.pow(10d, lin)
//
//     }
//     def worldToView(v1: Double) = {
//       val v = math.log10(v1)
//       (v - min2) / (max2 - min2) * width1
//     }
//     def min = min1
//     def max = max1
//   }
// }

case class AxisSettings(
    axisFactory: AxisFactory,
    numTicks: Int = 4,
    tickSpace: Option[Double] = None,
    baseTick: Option[Double] = None,
    numMinorTicksFactor: Int = 5,
    tickLength: RelFontSize = .35 fts,
    tickLabelDistance: RelFontSize = 1 fts,
    customTicks: Seq[(Double, String)] = Nil,
    labelRotation: Double = 0,
    width: RelFontSize = 20 fts,
    fontSize: RelFontSize = 1 fts,
    tickAlignment: Double = -1.0,
    lineWidth: Double = 1.0,
    lineLengthFraction: Double = 1d,
    lineStartFraction: Double = 0.0
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
            Shape.line(Point(view, 0d),
                       Point(view, tickAlignment * tickLength)),
            stroke = Some(Stroke(lineWidth))
          ),
          transform(
            transform(
              transform(
                TextBox(text, Point(view, 0.0), fontSize = fontSize),
                (b: Bounds) =>
                  AffineTransform.rotate(labelRotation, b.x, b.centerY)
              ),
              (b: Bounds) => AffineTransform.translate(0, tickLabelDistance)
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
            Shape.line(Point(0d, view),
                       Point(-1 * tickAlignment * tickLength, view)),
            stroke = Some(Stroke(lineWidth))
          ),
          transform(
            transform(
              transform(
                TextBox(text, Point(0.0, view), fontSize = fontSize),
                (b: Bounds) =>
                  AffineTransform.rotate(labelRotation, b.x + b.w, b.centerY)
              ),
              (b: Bounds) =>
                AffineTransform.translate(-1 * tickLabelDistance - b.w, 0)
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
          Shape.line(Point(view, 0d),
                     Point(view, tickLength * 0.5 * tickAlignment)),
          stroke = Some(Stroke(lineWidth))
        )
      else
        ShapeElem(
          Shape.line(Point(0d, view),
                     Point(-1 * tickLength * 0.5 * tickAlignment, view)),
          stroke = Some(Stroke(lineWidth))
        )
    }

    val lineStart = lineStartFraction * axis.width
    val lineEnd = lineStart + axis.width * lineLengthFraction

    val line =
      if (horizontal)
        ShapeElem(
          Shape.line(Point(lineStart, 0d), Point(lineEnd, 0d)),
          stroke = Some(Stroke(lineWidth))
        )
      else
        ShapeElem(
          Shape.line(Point(0d, lineStart), Point(0d, lineEnd)),
          stroke = Some(Stroke(lineWidth))
        )

    val extra =
      customTicks
        .filter(i => i._1 >= axis.min && i._1 <= axis.max)
        .map(i => makeTick(i._1, i._2))

    val (majorTicks1, minorTicks1) =
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

    val majorTickElems = sequence(
      majorTicks
        .map(w => makeTick(w, if (w == 0.0) "0" else f"$w%.2g")) ++ extra
    )

    val minorTickElems = sequence(minorTicks.map(makeMinorTick))

    (majorTicks,
     customTicks.map(_._1).toList,
     group(line, majorTickElems, minorTickElems, FreeLayout))

  }
}
