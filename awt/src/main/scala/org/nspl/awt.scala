package org.nspl

import java.awt.Graphics2D
import java.awt.{Font => JFont}
import java.text.AttributedString
import java.awt.font.LineBreakMeasurer

import JavaFontConversion._

case class JavaRC(graphics: Graphics2D) extends RenderingContext

object awtrenderer extends JavaAWTUtil {

  implicit val defaultGlyphMeasurer = AwtGlyphMeasurer

  implicit val defaultAWTFont: FontConfiguration = importFont("Arial")

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

    def render(ctx: JavaRC, elem: TextBox): Unit = {
      if (elem.text.size > 0) {
        savePaint(ctx.graphics) { graphics =>
          saveStroke(graphics) { graphics2 =>
            // graphics2.draw(elem.bounds)
            graphics2.setPaint(elem.color)

            val font: JFont = elem.font

            val frc = graphics2.getFontRenderContext()

            def getOutline(text: String) =
              new java.awt.font.TextLayout(text, font, frc)
                .getOutline(new java.awt.geom.AffineTransform())

            elem.layout.lines.foreach {
              case (line, lineTx) =>
                val shape = getOutline(line)

                val tx = elem.txLoc.concat(lineTx)

                graphics2.fill(tx.createTransformedShape(shape))
            }
          }
        }
      }
    }
  }

}
