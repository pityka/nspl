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
      breakWidth: Double,
      graphics: Graphics2D,
      x: Double,
      y: Double
    ): Iterator[java.awt.Shape] = {
      val attributedString = new AttributedString(text)
      attributedString.addAttribute(java.awt.font.TextAttribute.FONT, font);
      val paragraph = attributedString.getIterator();
      val paragraphStart = paragraph.getBeginIndex();
      val paragraphEnd = paragraph.getEndIndex();
      val frc = graphics.getFontRenderContext();
      val lineMeasurer = new LineBreakMeasurer(paragraph, frc);
      var drawPosY = y;
      lineMeasurer.setPosition(paragraphStart);

      new Iterator[java.awt.Shape] {
        def hasNext = lineMeasurer.getPosition() < paragraphEnd
        def next = {
          val layout = lineMeasurer.nextLayout(breakWidth.toFloat);
          val drawPosX = (if (layout.isLeftToRight()) 0 else breakWidth - layout.getAdvance) + x
          drawPosY += layout.getAscent();
          val shape = layout.getOutline(java.awt.geom.AffineTransform.getTranslateInstance(drawPosX, drawPosY))
          drawPosY += layout.getDescent() + layout.getLeading();
          shape
        }
      }

    }

    def render(ctx: JavaRC, elem: TextBox): Unit = {
      if (elem.text.size > 0) {
        savePaint(ctx.graphics) { graphics =>
          saveStroke(graphics) { graphics2 =>
            // graphics2.draw(elem.bounds)
            graphics2.setPaint(elem.color)
            getTextLayouts(
              elem.text,
              new Font(Font.MONOSPACED, Font.PLAIN, elem.fontSize.toInt),
              elem.width,
              graphics2,
              elem.loc.x,
              elem.loc.y
            ).foreach { shape =>
                graphics2.fill(elem.tx.createTransformedShape(shape))
              }
          }
        }
      }
    }
  }

}
