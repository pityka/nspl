package org.nspl

/** Describes the name and size of the font set used to draw letters
  *
  * This description is independent of the rendering context. Each rendering
  * context has to provide a GlyphMeasurer to compute the concrete space
  * occupied by the given Font (font name and font size)
  */
class Font(val name: String, val size: Int)

/** A Font paired with a GlyphMeasurer forms a FontConfiguration */
class FontConfiguration(val font: Font, measure: Font.GlyphMeasurer) {
  def advance(c: Char) = measure.advance(c, font)
  val lineMetrics = measure.lineMetrics(font)
}

object Font {

  /** A GlyphMeasurer can compute line metrics from a font. Implementations of
    * this trait are specific to a rendering context
    */
  trait GlyphMeasurer {
    def advance(s: Char, f: Font): Double
    def lineMetrics(f: Font): LineMetrics
  }

  /** Holds line metrics data
    *
    * For the meaning of the members see
    * https://docs.oracle.com/javase/tutorial/2d/text/fontconcepts.html
    *
    * Ascent is the distance from the baseline to the typical highest point of
    * the letter (ascender line). Descent is the distance from the baseline to
    * the typical lowest point of the letter (descender line). Leading is the
    * gap between lines, i.e. distance from the descender line to the next
    * line's ascender line.
    */
  case class LineMetrics(ascent: Double, descent: Double, leading: Double)
}

/** Holds a text layout
  *
  * @param lines
  *   a sequence of lines. Each line is a string with the characters of the line
  *   and an AffineTransformation with the line's displacement.
  * @param bounds
  *   outer bounding box of the layout
  */
case class TextLayout(lines: Seq[(String, AffineTransform)], bounds: Bounds) {
  def isEmpty = lines.isEmpty || lines.forall(_._1.isEmpty)
}

object TextLayout {

  /** factory method for text layouts
    *
    * Given an optional max width, complete text and a font size it computes the
    * text layout,
    * i.e. breaks the text into lines
    *
    * It breaks lines on space, tab and newline characters.
    */
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
