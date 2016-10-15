package org.nspl

import java.awt.Graphics2D
import java.awt.Font
import java.text.AttributedString
import java.awt.font.LineBreakMeasurer

case class JavaRC(graphics: Graphics2D) extends RenderingContext

object awtrenderer extends JavaAWTUtil {

  implicit val shapeRenderer = new AER[ShapeElem] {
    def render(ctx: JavaRC, elem: ShapeElem): Unit = {
      savePaint(ctx.graphics) { graphics =>
        saveStroke(graphics) { graphics2 =>
          if (elem.fill.a > 0.0) {
            graphics2.setPaint(elem.fill)
            graphics2.fill(elem.shape)
          }
          if (elem.stroke.isDefined && elem.strokeColor.a > 0) {
            graphics2.setStroke(elem.stroke.get)
            graphics2.setPaint(elem.strokeColor)
            graphics2.draw(elem.shape)
          }
        }
      }
    }
  }

  implicit val textRenderer = new AER[TextBox] {

    def getTextLayouts(
      text: String,
      font: Font,
      maxCharInLine: Int,
      graphics: Graphics2D,
      x: Double,
      y: Double
    ): Iterator[java.awt.Shape] = {
      val frc = graphics.getFontRenderContext();
      var drawPosY = y;

      text.toSeq.grouped(maxCharInLine).map(_.mkString).map { line =>
        val attributedString = new AttributedString(line)
        attributedString.addAttribute(java.awt.font.TextAttribute.FONT, font);
        val paragraph = attributedString.getIterator();
        val paragraphStart = paragraph.getBeginIndex();
        val paragraphEnd = paragraph.getEndIndex();
        val lineMeasurer = new LineBreakMeasurer(paragraph, frc);
        lineMeasurer.setPosition(paragraphStart);
        val layout = lineMeasurer.nextLayout(Float.MaxValue)
        val drawPosX = x
        drawPosY += layout.getAscent();
        val shape = layout.getOutline(java.awt.geom.AffineTransform.getTranslateInstance(drawPosX, drawPosY))
        drawPosY += layout.getDescent() + layout.getLeading();
        shape
      }

    }

    def render(ctx: JavaRC, elem: TextBox): Unit = {
      if (elem.text.size > 0) {
        savePaint(ctx.graphics) { graphics =>
          saveStroke(graphics) { graphics2 =>
            // graphics2.draw(elem.bounds)
            graphics2.setPaint(elem.color)
            val it = getTextLayouts(
              elem.text,
              new Font(Font.MONOSPACED, Font.PLAIN, 12),
              elem.maxCharInLine,
              graphics2,
              0d, 0d
            )
            val first = it.next
            (List(first).iterator ++ it).foreach { shape =>
              graphics2.fill(elem.tx.concat(
                AffineTransform.translate(
                  elem.loc.x,
                  elem.loc.y
                )
                  .concat(
                    AffineTransform.scale(
                      elem.lineWidth / first.getBounds.getWidth,
                      elem.lineWidth / first.getBounds.getWidth
                    )
                  )
              ).createTransformedShape(shape))
            }
          }
        }
      }
    }
  }

}
