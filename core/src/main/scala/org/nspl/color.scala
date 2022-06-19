package org.nspl

import scala.language.existentials

trait Colormap { self =>
  def apply(v: Double): Color
  def withRange(min: Double, max: Double): Colormap
}
object Colormap {
  def map(cm: Colormap)(f: Color => Color) = new Colormap {
    def apply(v: Double) = f(cm.apply(v))
    def withRange(min: Double, max: Double) = this
  }
}
case class Color(r: Int, g: Int, b: Int, a: Int) extends Colormap {
  def apply(v: Double) = this
  def withRange(min: Double, max: Double) = this
}
object Color {
  def apply(r: Int, g: Int, b: Int): Color = Color(r, g, b, 255)
  val black = Color(0, 0, 0, 255)
  val white = Color(255, 255, 255, 255)
  val transparent = Color(0, 0, 0, 0)
  val red = Color(255, 0, 0, 255)
  val blue = Color(0, 0, 255, 255)
  val green = Color(0, 255, 0, 255)
  val gray1 = Color(50, 50, 50, 255)
  val gray2 = Color(100, 100, 100, 255)
  val gray3 = Color(150, 150, 150, 255)
  val gray4 = Color(200, 200, 200, 255)
  val gray5 = Color(220, 220, 220, 255)
  val gray6 = Color(230, 230, 230, 255)
  val grey1 = Color(50, 50, 50, 255)
  val grey2 = Color(100, 100, 100, 255)
  val grey3 = Color(150, 150, 150, 255)
  val grey4 = Color(200, 200, 200, 255)
  val grey5 = Color(220, 220, 220, 255)
  val grey6 = Color(230, 230, 230, 255)
  val BLACK = black
  val WHITE = white
  val RED = red
  val GREEN = green
  val BLUE = blue

  private def linearInterpolation(x0: Int, x1: Int, a: Double): Int =
    (x0 + (x1 - x0) * a).toInt

  def rgbInterpolate(x0: Color, x1: Color, a: Double): Color =
    Color(
      linearInterpolation(x0.r, x1.r, a),
      linearInterpolation(x0.g, x1.g, a),
      linearInterpolation(x0.b, x1.b, a),
      linearInterpolation(x0.a, x1.a, a)
    )

}

case class TableColormap(map: Map[Double, Color]) extends Colormap {
  def apply(v: Double) = map.get(v).getOrElse(Color.gray5)
  def withRange(min: Double, max: Double) = this
}

object TableColormap {
  def apply(colors: Color*): TableColormap = TableColormap(
    colors.zipWithIndex.map(x => x._2.toDouble -> x._1).toMap
  )
}

case class HeatMapColors(
    min: Double = 0.0,
    max: Double = 1.0,
    center: Option[Double] = None
) extends Colormap {

  def apply(value: Double): Color = if (value.isNaN()) Color.transparent
  else {

    val c = center.getOrElse((min + max) * 0.5)

    val v =
      if (value > max) 1.0
      else if (value < min) 0.0
      else if (value > c) 0.5 * (value - c) / (max - c) + 0.5
      else (value - min) * 0.5 / (c - min)

    def scaleHue(v: Double) = (2.0 / 3.0) - v * (2.0 / 3.0)

    val (r, g, b) = hsl2rgb2(scaleHue(v), 1d, 0.5d)

    Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, 255)
  }

  def withRange(min: Double, max: Double) = HeatMapColors(min, max, center)
}

case class GrayScale(
    min: Double = 0.0,
    max: Double = 1.0,
    white: Int = 255,
    transparentBelowBounds: Boolean = false
) extends Colormap {

  def apply(value: Double): Color = if (value.isNaN()) Color.transparent
  else {

    val v =
      if (value > max) 1.0
      else if (value < min) 0.0
      else (value - min) / (max - min)

    val alpha =
      if (transparentBelowBounds && value < min) 0 else 255

    Color(
      white - (v * white).toInt,
      white - (v * white).toInt,
      white - (v * white).toInt,
      alpha
    )
  }

  def withRange(min: Double, max: Double) = GrayScale(min, max, white)
}

case class RedBlue(
    min: Double = 0.0,
    max: Double = 1.0,
    center: Option[Double] = None,
    centerBrightness: Int = 255
) extends Colormap {

  def apply(value: Double): Color = if (value.isNaN()) Color.transparent
  else {

    val c = center.getOrElse((max + min) * 0.5)

    val cColor = Color(centerBrightness, centerBrightness, centerBrightness)

    if (value > c) {
      val v =
        if (value > max) 1.0
        else if (value < min) 0.0
        else (value - c) / (max - c)
      Color.rgbInterpolate(cColor, Color.red, v)
    } else {
      val v =
        if (value < min) 1.0
        else if (value > max) 0.0
        else (c - value) / (c - min)
      Color.rgbInterpolate(cColor, Color.blue, v)
    }

  }

  def withRange(min: Double, max: Double) =
    RedBlue(min, max, center, centerBrightness)
}

case class LogHeatMapColors(min: Double = 0.0, max: Double = 1.0)
    extends Colormap {

  val min1 = 1d
  val max1 = (max - min) + 1

  def apply(value: Double): Color = if (value.isNaN) Color.transparent
  else {

    val v =
      if (value > max) 1.0
      else if (value < min) 0d
      else math.log10(value - min + 1) / math.log10(max1)

    def scaleHue(v: Double) = (2.0 / 3.0) - v * (2.0 / 3.0)

    val (r, g, b) = hsl2rgb2(scaleHue(v), 1d, 0.5d)

    Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, 255)
  }

  def withRange(min: Double, max: Double) = LogHeatMapColors(min, max)
}

case class DiscreteColors(
    numColors: Int,
    saturation: Double = 1d,
    lighting: Double = 0.5
) extends Colormap {

  def apply(value: Double): Color = if (value.isNaN) Color.transparent
  else {
    val v: Double =
      if (value > numColors) numColors.toDouble
      else if (value < 0) 0d
      else value
    colorPick(v.toInt, numColors, saturation, lighting)
  }

  def withRange(min: Double, max: Double) = DiscreteColors(max.toInt)
}

private[nspl] trait Colors {

  def colorFromHexString(s: String): Color = {
    import java.lang.Integer.parseInt

    Color(
      parseInt(s.take(2), 16),
      parseInt(s.drop(2).take(2), 16),
      parseInt(s.drop(4).take(2), 16)
    )
  }

  def colorPick(
      idx: Int,
      numColors: Int
  ): Color = colorPick(idx, numColors, 1d, 0.5)

  def colorPick(
      idx: Int,
      numColors: Int,
      saturation: Double,
      lighting: Double
  ): Color = {
    if (idx >= numColors) hslCircle(idx, idx, saturation, lighting)
    else if (numColors <= shortColorList.size) shortColorList(idx)
    else if (numColors <= longColorList.size) longColorList(idx)
    else hslCircle(idx, math.max(0, numColors - 1), saturation, lighting)
  }

  private[nspl] def hslCircle(idx: Int, max: Int, saturation: Double, lighting: Double) = {
    val (r, g, b) = hsl2rgb2(idx.toDouble / max.toDouble, saturation, lighting)
    val color = Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt)
    color
  }

  // https://en.wikipedia.org/wiki/HSL_and_HSV#From_HSL
  def hsl2rgb2(h: Double, s: Double, l: Double) = {
    val h1 = h * 360
    val c = (1 - math.abs(2 * l - 1)) * s
    val hprime = h1 / 60
    val x = c * (1d - math.abs(hprime % 2 - 1))
    val (r1, g1, b1) =
      if (hprime < 1) (c, x, 0d)
      else if (hprime < 2) (x, c, 0d)
      else if (hprime < 3) (0d, c, x)
      else if (hprime < 4) (0d, x, c)
      else if (hprime < 5) (x, 0d, c)
      else if (hprime < 6) (c, 0d, x)
      else (0d, 0d, 0d)

    val m = l - 0.5 * c
    (r1 + m, g1 + m, b1 + m)
  }

  private val shortColorList = Vector(
    Color(0, 0, 0, 255),
    Color(230, 50, 0, 255),
    Color(86, 180, 233, 255),
    Color(0, 158, 115, 255),
    Color(240, 228, 66, 255),
    Color(213, 94, 0, 255),
    Color(20, 4121, 167, 255)
  )

  private val longColorList = Vector(
    Color(230, 26, 75),
    Color(60, 180, 75),
    Color(255, 225, 25),
    Color(0, 130, 200),
    Color(245, 130, 48),
    Color(70, 240, 240),
    Color(240, 50, 230),
    Color(250, 190, 212),
    Color(0, 128, 128),
    Color(220, 190, 255),
    Color(170, 110, 40),
    Color(255, 250, 200),
    Color(128, 0, 0),
    Color(170, 255, 195),
    Color(0, 0, 128),
    Color(128, 128, 128),
    Color(0, 0, 0)
  )

}
