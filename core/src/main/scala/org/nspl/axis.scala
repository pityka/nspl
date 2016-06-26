package org.nspl

trait Axis {
  def viewToWorld(v: Double): Double
  def worldToView(v: Double): Double
  def min: Double
  def max: Double
  def width = math.abs(worldToView(max) - worldToView(min))
}

trait AxisFactory {
  def make(min: Double, max: Double, width: Double): Axis
}

object LinearAxisFactory extends AxisFactory {
  def make(min1: Double, max1: Double, width1: Double) = new Axis {
    def viewToWorld(v: Double) = v / width1 * (max1 - min1)
    def worldToView(v: Double) = (v - min1) / (max1 - min1) * width1
    def min = min1
    def max = max1
  }
}
object Log10AxisFactory extends AxisFactory {
  def make(min1: Double, max1: Double, width1: Double) = new Axis {
    val min2 = math.log10(min1)
    val max2 = math.log10(max1)
    def viewToWorld(v: Double) = {
      val lin = v / width1 * (max2 - min2)
      math.pow(10d, lin)

    }
    def worldToView(v1: Double) = {
      val v = math.log10(v1)
      (v - min2) / (max2 - min2) * width1
    }
    def min = min1
    def max = max1
  }
}

case class AxisSettings(
    axisFactory: AxisFactory,
    numTicks: Int = 4,
    numMinorTicksFactor: Int = 5,
    tickLength: RelFontSize = .35 fts,
    tickLabelDistance: RelFontSize = 1 fts,
    customTicks: Seq[(Double, String)] = Nil,
    labelRotation: Double = 0,
    width: RelFontSize = 20 fts,
    fontSize: RelFontSize = 1 fts,
    tickAlignment: Double = -1.0
) {

  def renderable(
    axis: Axis,
    labelTransformation: Bounds => AffineTransform = _ => AffineTransform.identity
  ) = {

    import axis._

    val tickSpace = {
      val (mantissa, exponent) = scientific((axis.max - axis.min) / (numTicks))
      val rounded = (mantissa * 10d).round / 10d
      math.abs(rounded * math.pow(10d, exponent))
    }

    def makeTick(world: Double, text: String) = {
      val view = worldToView(world)
      group(
        ShapeElem(Shape.line(Point(view, 0d), Point(view, tickAlignment * tickLength)), stroke = Some(Stroke(2d))),
        translate(
          transform(
            TextBox(text, Point(view, 0.0), fontSize = fontSize, width = fontSize.value * 4),
            (b: Bounds) =>
              AffineTransform.translate(b.w * (-0.5), 0)
                .concat(labelTransformation(b)
                  .concat(AffineTransform.rotateCenter(labelRotation)(b)))
          ),
          0,
          tickLabelDistance
        ),
        FreeLayout
      )
    }

    def makeMinorTick(world: Double) = {
      val view = worldToView(world)
      ShapeElem(Shape.line(Point(view, 0d), Point(view, tickLength * 0.5 * tickAlignment)), stroke = Some(Stroke(1d)))
    }

    val line = ShapeElem(
      Shape.line(Point(0d, 0d), Point(axis.width, 0d)),
      stroke = Some(Stroke(2d))
    )

    val baseTick = {
      if (axis.min <= 0 && axis.max >= 0) 0d
      else {
        val (maxM, maxE) = scientific(axis.max)
        val minM = axis.min / math.pow(10d, maxE)
        ((maxM - minM) * 0.5 + minM).floor * math.pow(10d, maxE)
      }
    }

    val extra = customTicks filter (i => i._1 >= axis.min && i._1 <= axis.max) map { i => makeTick(i._1, i._2) }

    val majorTicks = if (numTicks == 0) Nil
    else
      ((0 to ((axis.max - baseTick) / tickSpace).toInt map (i => baseTick + i * tickSpace)) ++
        (1 to ((baseTick - axis.min) / tickSpace).toInt map (i => baseTick - i * tickSpace)))
        .filterNot(x => customTicks.map(_._1).contains(x))
        .toList
        .map(w => math.max(math.min(w, axis.max), axis.min))
        .distinct

    val minorTicks = if (numTicks == 0) Nil
    else
      ((0 to ((axis.max - baseTick) / (tickSpace / numMinorTicksFactor)).toInt map (i => baseTick + i * tickSpace / numMinorTicksFactor)) ++
        (1 to ((baseTick - axis.min) / (tickSpace / numMinorTicksFactor)).toInt map (i => baseTick - i * tickSpace / numMinorTicksFactor)))
        .filterNot(x => customTicks.map(_._1).contains(x))
        .toList
        .map(w => math.max(math.min(w, axis.max), axis.min))
        .filterNot(majorTicks.contains)
        .distinct


    val majorTickElems = sequence(
      majorTicks.map(w => makeTick(w, if (w == 0.0) "0" else f"$w%.2g")) ++ extra
    )

    val minorTickElems = sequence(minorTicks.map(makeMinorTick))

    group(line, majorTickElems, minorTickElems, FreeLayout)

  }
}
