package org.nspl

case class HeatMapColors(min: Double = 0.0, max: Double = 1.0) extends Colormap {

  def apply(value: Double): Color = {

    val v = if (value > max) max else if (value < min) min else (value - min) / (max - min)

    def scaleHue(v: Double) = (2.0 / 3.0) - v * (2.0 / 3.0)

    val (r, g, b) = hsl2rgb2(scaleHue(v), 1d, 0.5d)
    Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, 255)
  }

  def withRange(min: Double, max: Double) = HeatMapColors(min, max)
}

case class DiscreteColors(max: Int) extends Colormap {

  def apply(value: Double): Color = {
    val v = if (value > max) max else if (value < 0) 0 else value
    colorPick(v.toInt, max)
  }

  def withRange(min: Double, max: Double) = DiscreteColors(max.toInt)
}

trait Colors {

  def colorPick(idx: Int, max: Int) = {
    if (max <= colorList.size && idx < colorList.size) colorList(idx)
    else
      synchronized {
        randomColorStream(idx)
      }
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

  lazy val random = new scala.util.Random(1232432);
  lazy val randomColorStream = {
    def newRandomColor = {
      val hue = (1.0f + random.nextFloat()) / 2.0f
      val saturation = (1.0f + random.nextFloat()) / 2.0f
      val brightness = (1.0f + random.nextFloat()) / 2.0f
      val (r, g, b) = hsl2rgb2(hue, saturation, brightness)
      Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, 255)
    }
    def loop: Stream[Color] = newRandomColor #:: loop
    loop
  }

  lazy val colorList = Vector(
    Color(0, 0, 0, 255),
    Color(250, 0, 0, 255),
    Color(237, 111, 9, 255),
    Color(50, 0, 250, 255),
    Color(9, 135, 237, 255),
    Color(32, 110, 27, 255),
    Color(84, 204, 204, 255),
    Color(139, 196, 81, 255),
    Color(171, 0, 250, 255),
    Color(176, 224, 0, 255),
    Color(250, 0, 158, 255),
    Color(224, 180, 0, 255),
    Color(196, 121, 0, 255),
    Color(0, 252, 164, 255),
    Color(255, 172, 120, 255)
  )

}
