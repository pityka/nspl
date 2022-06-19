package org.nspl

import java.awt.Graphics2D
import java.awt.{Font => JFont}
import java.text.AttributedString
import java.awt.font.LineBreakMeasurer

import JavaFontConversion._

class JavaRC private[nspl] (
    private[nspl] val graphics: Graphics2D,
    private[nspl] val doRender: Boolean
) extends RenderingContext[JavaRC] {

  private[nspl] var paintColor: Color = Color.black
  private[nspl] var stroke: Stroke = Stroke(1d)
  private[nspl] var transform: AffineTransform = AffineTransform.identity
  private[nspl] var transformInGraphics: AffineTransform =
    AffineTransform.identity

  private[nspl] def withPaint[T](color: Color)(f: => T) = {
    val current = paintColor
    if (current != color) {
      paintColor = color
      graphics.setPaint(awtrenderer.col2col(color))
    }
    f
  }
  private[nspl] def withStroke[T](str: Stroke)(f: => T) = {
    val current = stroke
    if (current != str) {
      stroke = str
      graphics.setStroke(awtrenderer.str2str(str))
    }
    f
  }

  type LocalTx = AffineTransform

  override def getTransform: AffineTransform = transform
  def localToScala(tx: AffineTransform): AffineTransform = tx

  def concatTransform(tx: AffineTransform): Unit = {
    transform = transform.applyBefore(tx)
  }

  def setTransform(tx: LocalTx): Unit = {
    transform = tx
  }
  def setTransformInGraphics() = {
    if (transformInGraphics != transform) {
      transformInGraphics = transform
      graphics.setTransform(awtrenderer.tx2tx(transform))
    }
  }

}

object awtrenderer extends JavaAWTUtil {

  implicit val defaultGlyphMeasurer: Font.GlyphMeasurer= AwtGlyphMeasurer

  implicit val defaultAWTFont: FontConfiguration = font("Arial")

  implicit val shapeRenderer: Renderer[ShapeElem, JavaRC] =
    new Renderer[ShapeElem, JavaRC] {

      private def drawAndFill(ctx: JavaRC, elem: ShapeElem) = {

        if (
          elem.fill.a > 0d || (elem.stroke.isDefined && elem.strokeColor.a > 0)
        ) {
          ctx.setTransformInGraphics()

          val shape = elem.shape

          if (elem.fill.a > 0.0) {
            ctx.withPaint(elem.fill) {
              ctx.graphics.fill(shape2awt(shape))
            }
          }
          if (elem.stroke.isDefined && elem.strokeColor.a > 0) {
            ctx.withPaint(elem.strokeColor) {
              ctx.withStroke(elem.stroke.get) {
                ctx.graphics.draw(shape2awt(shape))
              }
            }
          }
        }
      }
      def render(ctx: JavaRC, elem: ShapeElem): Unit = {

        ctx.withTransform(elem.tx applyBefore elem.shape.currentTransform) {
          if (ctx.doRender) {
            drawAndFill(ctx, elem)
          }

        }

      }
    }

  implicit val textRenderer: Renderer[TextBox, JavaRC] =
    new Renderer[TextBox, JavaRC] {

      def render(ctx: JavaRC, elem: TextBox): Unit = {
        if (!elem.layout.isEmpty && elem.color.a > 0) {
          ctx.withTransform(elem.tx) {
            ctx.withPaint(elem.color) {
              ctx.graphics.setFont(font2font(elem.font))
              elem.layout.lines.foreach { case (line, lineTx) =>
                ctx.withTransform(lineTx) {
                  if (ctx.doRender) {
                    ctx.setTransformInGraphics()
                    ctx.graphics.drawString(line, 0, 0)
                  }
                }
              }

            }

          }
        }
      }

    }
}
