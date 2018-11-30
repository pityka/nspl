package org.nspl

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
  val BLACK = black
  val WHITE = white
  val RED = red
  val GREEN = green
  val BLUE = blue
}

case class ManualColor(map: Map[Double, Color]) extends Colormap {
  def apply(v: Double) = map.get(v).getOrElse(Color.gray5)
  def withRange(min: Double, max: Double) = this
}

case class HeatMapColors(min: Double = 0.0, max: Double = 1.0)
    extends Colormap {

  def apply(value: Double): Color = {

    val v =
      if (value > max) 1.0
      else if (value < min) 0.0
      else (value - min) / (max - min)

    def scaleHue(v: Double) = (2.0 / 3.0) - v * (2.0 / 3.0)

    val (r, g, b) = hsl2rgb2(scaleHue(v), 1d, 0.5d)

    Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, 255)
  }

  def withRange(min: Double, max: Double) = HeatMapColors(min, max)
}

case class GrayScale(min: Double = 0.0, max: Double = 1.0, white: Int = 255)
    extends Colormap {

  def apply(value: Double): Color = {

    val v =
      if (value > max) 1.0
      else if (value < min) 0.0
      else (value - min) / (max - min)

    Color(white - (v * white).toInt,
          white - (v * white).toInt,
          white - (v * white).toInt,
          255)
  }

  def withRange(min: Double, max: Double) = GrayScale(min, max, white)
}

case class RedBlue(min: Double = 0.0, max: Double = 1.0, mid: Double = 0.5)
    extends Colormap {

  def apply(value: Double): Color = {

    val (r, g, b) = if (value > mid) {
      val v =
        if (value > max) 1.0
        else if (value < min) 0.0
        else (value - mid) / (max - mid)
      val v2 = 1 - v
      (1d, v2, v2)
    } else {
      val v =
        if (value < min) 1.0
        else if (value > max) 0.0
        else (mid - value) / (mid - min)
      val v2 = 1 - v
      (v2, v2, 1d)
    }

    Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, 255)
  }

  def withRange(min: Double, max: Double) = RedBlue(min, max, mid)
}

case class LogHeatMapColors(min: Double = 0.0, max: Double = 1.0)
    extends Colormap {

  val min1 = 1d
  val max1 = (max - min) + 1

  def apply(value: Double): Color = {

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
      myColorStream(idx)
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

  lazy val myColorStream = {

    def nextP(p: Int) = {
      def isPrime(number: Int): Boolean =
        if (number == 2 || number == 3) true
        else if (number % 2 == 0 || number % 3 == 0) false
        else {
          var divisor = 6
          var b = true
          while (divisor * divisor - 2 * divisor + 1 <= number && b) {

            if (number % (divisor - 1) == 0)
              b = false

            if (number % (divisor + 1) == 0)
              b = false

            divisor += 6
          }
          b
        }

      def loop(i: Int): Int = if (isPrime(i)) i else loop(i + 1)

      loop(p + 1)

    }

    def nextQ(p: Int, q: Int): (Int, Int) = {
      val q1 = if (p + 1 == q) nextP(q) else q
      val p1 = if (q == q1) p + 1 else 1
      (p1, q1)
    }

    def next(p: Int, q: Int, bb: Int): (Int, Int, Int, Color) = {
      val (hueP, hueQ) = if (bb == 3) nextQ(p, q) else (p, q)
      val b1: Int = if (bb == 3) 1 else bb + 1
      val hue = hueP.toDouble / hueQ
      val saturation = 1.0
      val brightness = (b1 / 3d) * 0.9
      val (r, g, b) = hsl2rgb2(hue, saturation, brightness)
      val color = Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, 255)
      (hueP, hueQ, b1, color)
    }

    def loop(p: Int, q: Int, b: Int): Stream[Color] = {
      val (p1, q1, b1, c) = next(p, q, b)
      c #:: loop(p1, q1, b1)
    }
    loop(0, 1, 0)
  }

  lazy val random = new scala.util.Random(1232432);
  lazy val randomColorStream = {
    def newRandomColor = {
      val hue = (random.nextFloat())
      val saturation = 0.6 + random.nextFloat() * 0.4
      val brightness = 0.4 + random.nextFloat() * 0.3
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
    Color(255, 172, 120, 255),
    Color(111, 178, 172, 255),
    Color(96, 56, 40, 255),
    Color(185, 86, 74, 255),
    Color(129, 105, 185, 255),
    Color(26, 25, 112, 255),
    Color(0, 112, 99, 255)
  )

}
