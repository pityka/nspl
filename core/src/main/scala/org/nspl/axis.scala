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
        def min = min1
        def max = max1
        def horizontal = true
      }
    else new Axis {
      def viewToWorld(v: Double) = (width1 - v) / width1 * (max1 - min1)
      def worldToView(v: Double) = width1 - (v - min1) / (max1 - min1) * width1
      def min = min1
      def max = max1
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
    lineWidth: Double = 1.0
)(implicit fc: FontConfiguration) {

  def renderable(
    axis: Axis,
    disableTicksAt: List[Double] = Nil
  ): (List[Double], AxisElem) = {

    import axis._

    val horizontal = axis.horizontal

    val tickSpace1 = tickSpace.getOrElse {
      val (mantissa, exponent) = scientific((axis.max - axis.min) / (numTicks))
      val rounded = (mantissa * 10d).round / 10d
      math.abs(rounded * math.pow(10d, exponent))
    }

    val numTicks1 = if (tickSpace.isEmpty) numTicks else (axis.max - axis.min) / tickSpace1

    val baseTick1 = baseTick getOrElse {
      if (axis.min <= 0 && axis.max >= 0) 0d
      else {
        val (maxM, maxE) = scientific(axis.max)
        val minM = axis.min / math.pow(10d, maxE)
        ((maxM - minM) * 0.5 + minM).floor * math.pow(10d, maxE)
      }
    }

    def makeTick(world: Double, text: String) = {
      val view = worldToView(world)
      if (horizontal)
        group(
          ShapeElem(
            Shape.line(Point(view, 0d), Point(view, tickAlignment * tickLength)),
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
            (b: Bounds) => AffineTransform.translate(if (labelRotation == 0.0) b.w * (-0.5) else 0, 0)
          ), FreeLayout
        )
      else
        group(
          ShapeElem(
            Shape.line(Point(0d, view), Point(-1 * tickAlignment * tickLength, view)),
            stroke = Some(Stroke(lineWidth))
          ),
          transform(
            transform(
              transform(
                TextBox(text, Point(0.0, view), fontSize = fontSize),
                (b: Bounds) =>
                  AffineTransform.rotate(labelRotation, b.x + b.w, b.centerY)
              ),
              (b: Bounds) => AffineTransform.translate(-1 * tickLabelDistance - b.w, 0)
            ),
            (b: Bounds) => AffineTransform.translate(0, if (labelRotation == 0.0) b.h * (-0.5) else 0)
          ),
          FreeLayout
        )
    }

    def makeMinorTick(world: Double) = {
      val view = worldToView(world)
      if (horizontal)
        ShapeElem(
          Shape.line(Point(view, 0d), Point(view, tickLength * 0.5 * tickAlignment)),
          stroke = Some(Stroke(1d))
        )
      else
        ShapeElem(
          Shape.line(Point(0d, view), Point(-1 * tickLength * 0.5 * tickAlignment, view)),
          stroke = Some(Stroke(1d))
        )
    }

    val line = if (horizontal)
      ShapeElem(
        Shape.line(Point(0d, 0d), Point(axis.width, 0d)),
        stroke = Some(Stroke(lineWidth))
      )
    else ShapeElem(
      Shape.line(Point(0d, 0d), Point(0d, axis.width)),
      stroke = Some(Stroke(lineWidth))
    )

    val extra =
      customTicks
        .filter(i => i._1 >= axis.min && i._1 <= axis.max)
        .map(i => makeTick(i._1, i._2))

    val majorTicks = if (numTicks1 == 0) Nil
    else
      ((0 to ((axis.max - baseTick1) / tickSpace1).toInt map (i =>
        baseTick1 + i * tickSpace1)) ++
        (1 to ((baseTick1 - axis.min) / tickSpace1).toInt map (i =>
          baseTick1 - i * tickSpace1)))
        .filterNot(x => customTicks.map(_._1).contains(x))
        .filterNot(x => disableTicksAt.contains(x))
        .toList
        .map(w => math.max(math.min(w, axis.max), axis.min))
        .distinct

    val minorTicks = if (numTicks1 == 0 || numMinorTicksFactor <= 0) Nil
    else
      ((0 to ((axis.max - baseTick1) / (tickSpace1 / numMinorTicksFactor)).toInt map (i =>
        baseTick1 + i * tickSpace1 / numMinorTicksFactor)) ++
        (1 to ((baseTick1 - axis.min) / (tickSpace1 / numMinorTicksFactor)).toInt map (i =>
          baseTick1 - i * tickSpace1 / numMinorTicksFactor)))
        .filterNot(x => customTicks.map(_._1).contains(x))
        .toList
        .map(w => math.max(math.min(w, axis.max), axis.min))
        .filterNot(majorTicks.contains)
        .distinct

    val majorTickElems = sequence(
      majorTicks.map(w => makeTick(w, if (w == 0.0) "0" else f"$w%.2g")) ++ extra
    )

    val minorTickElems = sequence(minorTicks.map(makeMinorTick))

    majorTicks -> group(line, majorTickElems, minorTickElems, FreeLayout)

  }
}
