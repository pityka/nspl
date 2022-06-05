package org.nspl

sealed trait Font {
  type F >: this.type <: Font
  def advance(c: Char, size: Int)(implicit fm: GlyphMeasurer[F]) =
    fm.advance(c, this)
  def size: Int
}

trait GlyphMeasurer[-F <: Font] {
  def advance(s: Char, f: F): Double
  def lineMetrics(f: F): LineMetrics
}

trait FontConfiguration {
  def font: Font
  def advance(c: Char): Double
  val lineMetrics: LineMetrics
}

case object Monospace extends Font with FontConfiguration {
  val font = this
  val size = 10
  def advance(c: Char) = FixGlyphMeasurer.advance(c, font)
  val lineMetrics = FixGlyphMeasurer.lineMetrics(font)
}

case class NamedFont(name: String, size: Int) extends Font

/* https://docs.oracle.com/javase/tutorial/2d/text/fontconcepts.html */
case class LineMetrics(ascent: Double, descent: Double, leading: Double)

object FixGlyphMeasurer extends GlyphMeasurer[Font#F] {
  def advance(s: Char, f: Font#F) = f.size.toDouble * 0.6
  def lineMetrics(f: Font#F) =
    LineMetrics(
      ascent = f.size.toDouble * 0.78,
      descent = f.size.toDouble * 0.22,
      leading = 0d
    )
}

case class GenericFontConfig[F <: Font](font: F)(implicit
    val measure: GlyphMeasurer[F#F]
) extends FontConfiguration {
  def advance(c: Char) = measure.advance(c, font)
  val lineMetrics = measure.lineMetrics(font)
}

case class TextLayout(lines: Seq[(String, AffineTransform)], bounds: Bounds) {
  def isEmpty = lines.isEmpty || lines.forall(_._1.isEmpty)
}

object TextLayout {

  def apply(
      maxWidth: Option[Double],
      text: String,
      fontSize: RelFontSize
  )(implicit fc: FontConfiguration): TextLayout =
    if (text.isEmpty)
      TextLayout(List("" -> AffineTransform.identity), Bounds(0, 0, 0, 0))
    else {

      val fontSizeFactor = fontSize.factor
      val scale = AffineTransform.scale(fontSizeFactor, fontSizeFactor)

      def advance(c: Char) = fc.advance(c)

      val lineMetrics = fc.lineMetrics
      val ascent = lineMetrics.ascent
      val descent = lineMetrics.descent
      val leading = lineMetrics.leading
      val lineHeight = ascent + descent + leading

      def advanceWithChar(b: Bounds, c: Char): Bounds =
        Bounds(b.x, b.y, b.w + advance(c), b.h)

      def measureLine(chars: List[Char]) =
        chars.foldLeft(Bounds(0, 0, 0, lineHeight))(advanceWithChar)

      val chars = text.toList

      def lineBreakAllowed(c: Char) = c == ' ' || c == '\t' || c == '\n'

      if (maxWidth.isEmpty) {
        val bounds = measureLine(chars)
        
        val tx = AffineTransform.translateThenScale(
          tx = bounds.x,
          ty = bounds.y + ascent,
          sx = fontSizeFactor,
          sy = fontSizeFactor
        )

        TextLayout(List(chars.mkString -> tx), scale.transform(bounds))
      } else {
        val lines = chars
          .foldLeft(List[(List[Char], Bounds)]()) { (lines, char) =>
            lines match {
              case Nil =>
                val line = (char :: Nil)
                (line -> measureLine(line)) :: Nil
              case (lc, lb) :: ls =>
                val nb = advanceWithChar(lb, char)
                if (nb.w > maxWidth.get) {
                  val firstBreakable = lc.indexWhere(lineBreakAllowed)
                  val (newLine1, oldLine1) = lc.splitAt(firstBreakable)
                  val newLine = char :: newLine1
                  val oldLine = oldLine1.dropWhile(lineBreakAllowed)
                  val cbNew = measureLine(newLine)
                  val cbOld = measureLine(oldLine)
                  (newLine -> cbNew.copy(y = cbNew.h + lb.y)) :: (
                    oldLine,
                    cbOld.copy(y = lb.y)
                  ) :: ls
                } else (char :: lc, nb) :: ls

            }
          }
          .map(x => x._1.reverse.mkString -> x._2)
          .reverse

        val outerBounds = outline(lines.iterator.map(_._2), anchor = None)

        val transformations = lines.map { case (text, bounds) =>
          val tx = AffineTransform.translateThenScale(
            tx = bounds.x,
            ty = bounds.y + ascent,
            sx = fontSizeFactor,
            sy = fontSizeFactor
          )
          (text, tx)
        }

        val scaledOuterBounds = scale.transform(outerBounds)

        TextLayout(transformations, scaledOuterBounds)
      }

    }

}
