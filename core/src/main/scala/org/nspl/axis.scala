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
      val (mantissa, exponent) = scientific((axis.max - axis.min) / (numTicks - 1))
      val rounded = (mantissa * 5d).round / 5d
      rounded * math.pow(10d, exponent)
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

    val extra = customTicks filter (i => i._1 >= axis.min && i._1 <= axis.max) map { i => makeTick(i._1, i._2) }
    val ticks = sequence(
      ((0 until numTicks toList) map { i =>
        val world = math.min(axis.min + i * tickSpace, axis.max)
        if (customTicks.map(_._1).contains(world)) None
        else Some {
          val text = if (world == 0.0) "0" else f"$world%.2g"
          makeTick(world, text)
        }
      }).filter(_.isDefined).map(_.get) ++ extra
    )

    val minorTicks = sequence(
      ((0 until (numTicks * numMinorTicksFactor) toList) map { i =>
        val world = math.min(axis.min + i * tickSpace / numMinorTicksFactor, axis.max)
        if (customTicks.map(_._1).contains(world)) None
        else Some {
          makeMinorTick(world)
        }
      }).filter(_.isDefined).map(_.get)
    )

    group(line, ticks, minorTicks, FreeLayout)

  }
}
