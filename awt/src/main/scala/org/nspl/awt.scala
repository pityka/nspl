package org.nspl

import java.awt.Graphics2D
import java.awt.font.TextAttribute

import JavaFontConversion._

class JavaRC private[nspl] (
    private[nspl] val graphics: Graphics2D,
    private[nspl] val doRender: Boolean,
    private[nspl] val textAsShapes: Boolean = false
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

  implicit val defaultAWTFont: FontConfiguration = font("SansSerif")

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

      private def deriveFont(
          base: java.awt.Font,
          bold: Boolean,
          oblique: Boolean,
          subScript: Boolean,
          superScript: Boolean,
          underline: Boolean
      ): java.awt.Font = {
        if (!bold && !oblique && !subScript && !superScript && !underline) base
        else {
          val map =
            new java.util.HashMap[
              java.awt.font.TextAttribute,
              java.lang.Object
            ]()
          if (bold) {
            map.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_EXTRABOLD)
          }
          if (oblique) {
            map.put(TextAttribute.POSTURE, TextAttribute.POSTURE_OBLIQUE)
          }
          if (underline) {
            map.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON)
          }
          if (subScript && !superScript) {
            map.put(TextAttribute.SUPERSCRIPT, TextAttribute.SUPERSCRIPT_SUB)
          } else if (superScript && !subScript) {
            map.put(TextAttribute.SUPERSCRIPT, TextAttribute.SUPERSCRIPT_SUPER)
          }
          base.deriveFont(map)
        }
      }

      def render(ctx: JavaRC, elem: TextBox): Unit = {
        if (!elem.layout.isEmpty && elem.color.a > 0) {
          ctx.withTransform(elem.tx) {
            ctx.withPaint(elem.color) {
              val baseFont = font2font(elem.font)
              val jfont = deriveFont(
                baseFont,
                elem.bold,
                elem.oblique,
                elem.subScript,
                elem.superScript,
                elem.underline
              )
              if (!ctx.textAsShapes) {
                ctx.graphics.setFont(jfont)
              }
              elem.layout.lines.foreach { case (line, lineTx) =>
                ctx.withTransform(lineTx) {
                  if (ctx.doRender) {
                    ctx.setTransformInGraphics()
                    if (ctx.textAsShapes) {
                      val frc = ctx.graphics.getFontRenderContext
                      val gv = jfont.createGlyphVector(frc, line)
                      ctx.graphics.fill(gv.getOutline(0f, 0f))
                    } else {
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
}
