package org.nspl

import scala.scalanative.native

class NanoVGContext(val ctx: native.Ptr[nvg.Context])

@native.extern
@native.link("nanovg_gl")
object nvg {

  @native.name("nvgCreateGL3")
  def createGL3(flags: native.CInt): native.Ptr[Context] = native.extern

  @native.name("nvgDeleteGL3")
  def deleteGL3(ctx: native.Ptr[Context]): Unit = native.extern

  @native.name("NVGcontext")
  type Context = native.Ptr[Byte]

  @native.name("NVGcolor")
  type Color = native.Ptr[Byte]

  @native.name("NVGpaint")
  type Paint = native.Ptr[Byte]

  @native.name("NVGcompositeOperationState")
  type CompositeOperationState = native.Ptr[Byte]

  @native.name("NVGglyphPosition")
  type GlyphPosition = native.Ptr[Byte]

  @native.name("NVGtextRow")
  type TextRow = native.Ptr[Byte]

  @native.name("nvgGlobalCompositeOperation")
  def globalCompositeOperation(ctx: native.Ptr[Context],
                               op: native.CInt): Unit = native.extern

  @native.name("nvgGlobalCompositeBlendFunc")
  def globalCompositeBlendFunc(ctx: native.Ptr[Context],
                               sourceFactor: native.CInt,
                               dstFactor: native.CInt): Unit = native.extern

  @native.name("nvgGlobalCompositeBlendFuncSeparate")
  def globalCompositeBlendFuncSeparate(ctx: native.Ptr[Context],
                                       srcRGB: native.CInt,
                                       dstRGB: native.CInt,
                                       srcAlpha: native.CInt,
                                       dstAlpha: native.CInt): Unit =
    native.extern

// Color

  @native.name("nvgRGBAf")
  def rgba(r: native.CFloat,
           g: native.CFloat,
           b: native.CFloat,
           a: native.CFloat): Color = native.extern

  @native.name("nvgRGBA")
  def rgba(r: native.UByte,
           g: native.UByte,
           b: native.UByte,
           a: native.UByte): Color = native.extern

  @native.name("nvgRGB")
  def rgb(r: native.UByte, g: native.UByte, b: native.UByte): Color =
    native.extern

  @native.name("nvgRGBf")
  def rgb(r: native.CFloat, g: native.CFloat, b: native.CFloat): Color =
    native.extern

  @native.name("nvgLerpRGBA")
  def lerpRGBA(c0: Color, c1: Color, u: native.CFloat): Color = native.extern

  @native.name("nvgTransRGBA")
  def transRGBA(c0: Color, u: native.UByte): Color = native.extern

  @native.name("nvgTransRGBAf")
  def transRGBA(c0: Color, u: native.CFloat): Color = native.extern

  @native.name("nvgHSL")
  def hsl(h: native.CFloat, s: native.CFloat, l: native.CFloat): Color =
    native.extern

  @native.name("nvgHSLA")
  def hsla(h: native.CFloat,
           s: native.CFloat,
           l: native.CFloat,
           a: native.UByte): Color = native.extern

// State

  @native.name("nvgSave")
  def save(ctx: native.Ptr[Context]): Unit = native.extern

  @native.name("nvgRestore")
  def restore(ctx: native.Ptr[Context]): Unit = native.extern

  @native.name("nvgReset")
  def reset(ctx: native.Ptr[Context]): Unit = native.extern

  // Render

  @native.name("nvgShapeAntiAlias")
  def shapeAntiAlias(ctx: native.Ptr[Context], enable: native.CInt): Unit =
    native.extern

  @native.name("nvgStrokeColor")
  def strokeColor(ctx: native.Ptr[Context], color: Color): Unit = native.extern

  @native.name("nvgStrokePaint")
  def strokePaint(ctx: native.Ptr[Context], color: Paint): Unit = native.extern

  @native.name("nvgFillColor")
  def fillColor(ctx: native.Ptr[Context], color: Color): Unit = native.extern

  @native.name("nvgFillPaint")
  def fillPaint(ctx: native.Ptr[Context], color: Paint): Unit = native.extern
  @native.name("nvgMiterLimit")
  def miterLimit(ctx: native.Ptr[Context], limit: native.CFloat): Unit =
    native.extern

  @native.name("nvgStrokeWidth")
  def strokeWidth(ctx: native.Ptr[Context], width: native.CFloat): Unit =
    native.extern

  @native.name("nvgLineCap")
  def lineCap(ctx: native.Ptr[Context], cap: native.CInt): Unit = native.extern

  @native.name("nvgLineJoin")
  def lineJoin(ctx: native.Ptr[Context], join: native.CInt): Unit =
    native.extern

  @native.name("nvgGlobalAlpha")
  def globalAlpha(ctx: native.Ptr[Context], alpha: native.CFloat): Unit =
    native.extern

// Transforms

  @native.name("nvgResetTransform")
  def resetTransform(ctx: native.Ptr[Context]): Unit = native.extern

//   [sx kx tx]
//   [ky sy ty]
//   [ 0  0  1]
  @native.name("nvgTransform")
  def transform(ctx: native.Ptr[Context],
                sx: native.CFloat,
                ky: native.CFloat,
                kx: native.CFloat,
                sy: native.CFloat,
                tx: native.CFloat,
                ty: native.CFloat): Unit = native.extern
  @native.name("nvgTranslate")
  def translate(ctx: native.Ptr[Context],
                tx: native.CFloat,
                ty: native.CFloat): Unit = native.extern

  @native.name("nvgRotate")
  def rotate(ctx: native.Ptr[Context], rad: native.CFloat): Unit = native.extern

  @native.name("nvgSkewX")
  def skewX(ctx: native.Ptr[Context], rad: native.CFloat): Unit = native.extern

  @native.name("nvgSkewY")
  def skewYctx(ctx: native.Ptr[Context], rad: native.CFloat): Unit =
    native.extern

  @native.name("nvgScale")
  def scale(ctx: native.Ptr[Context],
            sx: native.CFloat,
            sy: native.CFloat): Unit = native.extern

//   [a c e]
//   [b d f]
//   [0 0 1]
  @native.name("nvgCurrentTransform")
  def currentTransform(
      ctx: native.Ptr[Context],
      mat: native.Ptr[native.CArray[native.CFloat, native.Nat._6]]): Unit =
    native.extern

  @native.name("nvgTransformIdentity")
  def transformIdentity(
      mat: native.CArray[native.CFloat, native.Nat._6]): Unit = native.extern

  @native.name("nvgTransformTranslate")
  def transformTranslate(mat: native.CArray[native.CFloat, native.Nat._6],
                         tx: native.CFloat,
                         ty: native.CFloat): Unit = native.extern

  @native.name("nvgTransformScale")
  def transformScale(mat: native.CArray[native.CFloat, native.Nat._6],
                     sx: native.CFloat,
                     sy: native.CFloat): Unit = native.extern

  @native.name("nvgTransformRotate")
  def transformRotate(mat: native.CArray[native.CFloat, native.Nat._6],
                      rad: native.CFloat): Unit = native.extern

  @native.name("nvgTransformSkewX")
  def transformSkewX(mat: native.CArray[native.CFloat, native.Nat._6],
                     rad: native.CFloat): Unit = native.extern

  @native.name("nvgTransformSkewY")
  def transformSkewY(mat: native.CArray[native.CFloat, native.Nat._6],
                     rad: native.CFloat): Unit = native.extern

  @native.name("nvgTransformMultiply")
  def transformMultiply(
      dst: native.CArray[native.CFloat, native.Nat._6],
      src: native.CArray[native.CFloat, native.Nat._6]): Unit = native.extern

  @native.name("nvgTransformPremultiply")
  def transformPremultiply(
      dst: native.CArray[native.CFloat, native.Nat._6],
      src: native.CArray[native.CFloat, native.Nat._6]): Unit = native.extern

  @native.name("nvgTransformInverse")
  def transformInverse(
      dst: native.CArray[native.CFloat, native.Nat._6],
      src: native.CArray[native.CFloat, native.Nat._6]): native.CInt =
    native.extern

  @native.name("nvgTransformPoint")
  def transformPoint(dstx: native.Ptr[native.CFloat],
                     dsty: native.Ptr[native.CFloat],
                     transform: native.CArray[native.CFloat, native.Nat._6],
                     srcx: native.CFloat,
                     srcy: native.CFloat): Unit = native.extern

  @native.name("nvgDegToRad")
  def degToRad(deg: native.CFloat): native.CFloat = native.extern

  @native.name("nvgRadToDeg")
  def radToDeg(rad: native.CFloat): native.CFloat = native.extern

  // Images

  @native.name("nvgCreateImage")
  def createImage(ctx: native.Ptr[Context],
                  filename: native.CString,
                  flags: native.CInt): native.CInt = native.extern

  @native.name("nvgCreateImageMem")
  def createImageMem(ctx: native.Ptr[Context],
                     flags: native.CInt,
                     data: native.Ptr[native.UByte],
                     ndata: native.CInt): native.CInt = native.extern

  @native.name("nvgCreateImageRGBA")
  def createImageRGBA(ctx: native.Ptr[Context],
                      w: native.CInt,
                      h: native.CInt,
                      flags: native.CInt,
                      data: native.Ptr[native.UByte]): native.CInt =
    native.extern

  @native.name("nvgUpdateImage")
  def updateImage(ctx: native.Ptr[Context],
                  image: native.CInt,
                  data: native.Ptr[native.UByte]): native.CInt = native.extern

  @native.name("nvgImageSize")
  def imageSize(ctx: native.Ptr[Context],
                image: native.CInt,
                w: native.Ptr[native.CInt],
                h: native.Ptr[native.CInt]): native.CInt = native.extern

  @native.name("nvgDeleteImage")
  def deleteImage(ctx: native.Ptr[Context], image: native.CInt): native.CInt =
    native.extern

  // Paint

  @native.name("nvgLinearGradient")
  def linearGradient(ctx: native.Ptr[Context],
                     sx: native.CFloat,
                     sy: native.CFloat,
                     ex: native.CFloat,
                     ey: native.CFloat,
                     icol: Color,
                     ocol: Color): Paint = native.extern

  @native.name("nvgBoxGradient")
  def boxGradient(ctx: native.Ptr[Context],
                  x: native.CFloat,
                  y: native.CFloat,
                  w: native.CFloat,
                  h: native.CFloat,
                  r: native.CFloat,
                  f: native.CFloat,
                  icol: Color,
                  ocol: Color): Paint = native.extern

  @native.name("nvgRadialGradient")
  def radialGradient(ctx: native.Ptr[Context],
                     cx: native.CFloat,
                     cy: native.CFloat,
                     inr: native.CFloat,
                     outr: native.CFloat,
                     icol: Color,
                     ocol: Color): Paint = native.extern

  @native.name("nvgImagePattern")
  def imagePattern(ctx: native.Ptr[Context],
                   ox: native.CFloat,
                   oy: native.CFloat,
                   ex: native.CFloat,
                   ey: native.CFloat,
                   angle: native.CFloat,
                   image: native.CInt,
                   alpha: native.CFloat): Paint = native.extern

  // Scissor

  @native.name("nvgScissor")
  def scissor(ctx: native.Ptr[Context],
              x: native.CFloat,
              y: native.CFloat,
              w: native.CFloat,
              h: native.CFloat): Unit = native.extern

  @native.name("nvgIntersectScissor")
  def intersectScissor(ctx: native.Ptr[Context],
                       x: native.CFloat,
                       y: native.CFloat,
                       w: native.CFloat,
                       h: native.CFloat): Unit = native.extern

  @native.name("nvgResetScissor")
  def resetScissor(ctx: native.Ptr[Context]): Unit = native.extern

  // Paths

  @native.name("nvgBeginPath")
  def beginPath(ctx: native.Ptr[Context]): Unit = native.extern

  @native.name("nvgMoveTo")
  def moveTo(ctx: native.Ptr[Context],
             x: native.CFloat,
             y: native.CFloat): Unit = native.extern

  @native.name("nvgLineTo")
  def lineTo(ctx: native.Ptr[Context],
             x: native.CFloat,
             y: native.CFloat): Unit = native.extern

  @native.name("nvgBezierTo")
  def bezierTo(ctx: native.Ptr[Context],
               c1x: native.CFloat,
               c1y: native.CFloat,
               c2x: native.CFloat,
               c2y: native.CFloat,
               x: native.CFloat,
               y: native.CFloat): Unit = native.extern

  @native.name("nvgQuadTo")
  def quadTo(ctx: native.Ptr[Context],
             cx: native.CFloat,
             cy: native.CFloat,
             x: native.CFloat,
             y: native.CFloat): Unit = native.extern

  @native.name("nvgArcTo")
  def arcTo(ctx: native.Ptr[Context],
            x1: native.CFloat,
            y1: native.CFloat,
            x2: native.CFloat,
            y2: native.CFloat,
            radius: native.CFloat): Unit = native.extern

  @native.name("nvgClosePath")
  def closePath(ctx: native.Ptr[Context]): Unit = native.extern

  @native.name("nvgPathWinding")
  def pathWinding(ctx: native.Ptr[Context], winding: native.CInt): Unit =
    native.extern

  @native.name("nvgArc")
  def arc(ctx: native.Ptr[Context],
          cx: native.CFloat,
          cy: native.CFloat,
          r: native.CFloat,
          a0: native.CFloat,
          a1: native.CFloat,
          winding: native.CInt): Unit = native.extern

  @native.name("nvgRect")
  def rect(ctx: native.Ptr[Context],
           x: native.CFloat,
           y: native.CFloat,
           w: native.CFloat,
           h: native.CFloat): Unit = native.extern

  @native.name("nvgRoundedRect")
  def roundedRect(ctx: native.Ptr[Context],
                  x: native.CFloat,
                  y: native.CFloat,
                  w: native.CFloat,
                  h: native.CFloat,
                  r: native.CFloat): Unit = native.extern

  @native.name("nvgEllipse")
  def ellipse(ctx: native.Ptr[Context],
              cx: native.CFloat,
              cy: native.CFloat,
              rx: native.CFloat,
              ry: native.CFloat): Unit = native.extern

  @native.name("nvgCircle")
  def circle(ctx: native.Ptr[Context],
             cx: native.CFloat,
             cy: native.CFloat,
             r: native.CFloat): Unit = native.extern

  @native.name("nvgFill")
  def fill(ctx: native.Ptr[Context]): Unit = native.extern

  @native.name("nvgStroke")
  def stroke(ctx: native.Ptr[Context]): Unit = native.extern

  @native.name("nvgBeginFrame")
  def beginFrame(ctx: native.Ptr[Context],
                 windowWidth: native.CInt,
                 windowHeight: native.CInt,
                 devicePixelRatio: native.CFloat): Unit = native.extern

  @native.name("nvgCancelFrame")
  def cancelFrame(ctx: native.Ptr[Context]): Unit = native.extern

  @native.name("nvgEndFrame")
  def endFrame(ctx: native.Ptr[Context]): Unit = native.extern

  // Fonts

  @native.name("nvgCreateFont")
  def createFont(ctx: native.Ptr[Context],
                 name: native.CString,
                 filename: native.CString): native.CInt = native.extern

  @native.name("nvgCreateFontMem")
  def createFontMem(ctx: native.Ptr[Context],
                    name: native.CString,
                    data: native.Ptr[Byte],
                    ndata: native.CInt,
                    freeData: native.CInt): native.CInt = native.extern

  @native.name("nvgFindFont")
  def findFont(ctx: native.Ptr[Context], name: native.CString): native.CInt =
    native.extern

  @native.name("nvgAddFallbackFontId")
  def addFallbackFontId(ctx: native.Ptr[Context],
                        baseFont: native.CInt,
                        fallbackFont: native.CInt): native.CInt = native.extern
  @native.name("nvgAddFallbackFont")
  def addFallbackFont(ctx: native.Ptr[Context],
                      baseFont: native.CString,
                      fallbackFont: native.CString): native.CInt = native.extern

  @native.name("nvgFontSize")
  def fontSize(ctx: native.Ptr[Context], size: native.CFloat): Unit =
    native.extern

  @native.name("nvgFontBlur")
  def fontBlur(ctx: native.Ptr[Context], blur: native.CFloat): Unit =
    native.extern

  @native.name("nvgTextLetterSpacing")
  def textLetterSpacing(ctx: native.Ptr[Context],
                        spacing: native.CFloat): Unit = native.extern

  @native.name("nvgTextLineHeight")
  def textLineHeight(ctx: native.Ptr[Context],
                     lineHeight: native.CFloat): Unit = native.extern

  @native.name("nvgTextAlign")
  def textAlign(ctx: native.Ptr[Context], align: native.CInt): Unit =
    native.extern

  @native.name("nvgFontFaceId")
  def fontFaceId(ctx: native.Ptr[Context], font: native.CInt): Unit =
    native.extern

  @native.name("nvgFontFace")
  def fontFace(ctx: native.Ptr[Context], font: native.CString): Unit =
    native.extern

  @native.name("nvgText")
  def text(ctx: native.Ptr[Context],
           x: native.CFloat,
           y: native.CFloat,
           string: native.CString,
           end: native.CString): Unit = native.extern
  @native.name("nvgTextBox")
  def textBox(ctx: native.Ptr[Context],
              x: native.CFloat,
              y: native.CFloat,
              breakRowWidth: native.CFloat,
              string: native.CString,
              end: native.CString): Unit = native.extern

  @native.name("nvgTextBounds")
  def textBounds(ctx: native.Ptr[Context],
                 x: native.CFloat,
                 y: native.CFloat,
                 string: native.CString,
                 end: native.CString,
                 bounds: native.CArray[native.CFloat, native.Nat._4]): Unit =
    native.extern

  @native.name("nvgTextBoxBounds")
  def textBoxBounds(ctx: native.Ptr[Context],
                    x: native.CFloat,
                    y: native.CFloat,
                    breakRowWidth: native.CFloat,
                    string: native.CString,
                    end: native.CString,
                    bounds: native.CArray[native.CFloat, native.Nat._4]): Unit =
    native.extern

  @native.name("nvgTextMetrics")
  def textMetrics(ctx: native.Ptr[Context],
                  ascender: native.Ptr[native.CFloat],
                  descender: native.Ptr[native.CFloat],
                  lineheight: native.Ptr[native.CFloat]): Unit = native.extern

  @native.name("nvgTextGlyphPositions")
  def textGlyphPositions(ctx: native.Ptr[Context],
                         x: native.CFloat,
                         y: native.CFloat,
                         string: native.CString,
                         end: native.CString,
                         positions: native.Ptr[GlyphPosition],
                         max: native.CInt): native.CInt = native.extern

  @native.name("nvgTextBreakLines")
  def textBreakLines(ctx: native.Ptr[Context],
                     string: native.CString,
                     end: native.CString,
                     breakRowWidth: native.CFloat,
                     positions: native.Ptr[TextRow],
                     max: native.CInt): native.CInt = native.extern

}

object Constants {
  object CreateFlags {
    val AntiAlias = 1 << 0
    val StencilStrokes = 1 << 1
    val Debug = 1 << 2
  }

  object Winding {
    val CounterClockWise = 1
    val ClockWise = 2
  }

  object Solidity {
    val Solid = 1
    val Hole = 2
  }

  object LineCap {
    val Butt = 0
    val Round = 1
    val Square = 2
    val Bevel = 3
    val Miter = 4
  }

  object Align {
    val Left = 1 << 0
    val Center = 1 << 1
    val Right = 1 << 2
    val Top = 1 << 3
    val Middle = 1 << 4
    val Bottom = 1 << 5
    val Baseline = 1 << 6
  }

  object BlendFactor {
    val Zero = 1 << 0
    val One = 1 << 1
    val SrcColor = 1 << 2
    val OneMinusSrcColor = 1 << 3
    val DstColor = 1 << 4
    val OneMinusDstColor = 1 << 5
    val SrcAlpha = 1 << 6
    val OneMinusSrcAlpha = 1 << 7
    val DstAlpha = 1 << 8
    val OneMinusDstAlpha = 1 << 9
    val SrcAlphaSaturate = 1 << 10
  }

  object CompositeOperation {
    val SourceOver = 0
    val SourceIn = 1
    val SourceOut = 2
    val Atop = 3
    val DestinationOver = 4
    val DestinationIn = 5
    val DestinationOut = 6
    val DestinationAtop = 7
    val Lighter = 8
    val Copy = 9
    val Xor = 10
  }

  object ImageFlags {
    val GenerateMipmaps = 1 << 0
    val Repeatx = 1 << 1
    val Repeaty = 1 << 2
    val Flipy = 1 << 3
    val Premultiplied = 1 << 4
    val Nearest = 1 << 5
  }
}
@native.extern
@native.link("glfw3")
object glfw3 {

  @native.name("GLFWmonitor")
  type Monitor = native.Ptr[Byte]

  @native.name("GLFWwindow")
  type Window = native.Ptr[Byte]

  @native.name("glfwWindowHint")
  def glfwWindowHint(flags: native.CInt, value: native.CInt): Unit =
    native.extern

  @native.name("glfwInit")
  def glfwInit(): Unit = native.extern

  @native.name("glfwTerminate")
  def glfwTerminate(): Unit = native.extern

  @native.name("glfwPollEvents")
  def glfwPollEvents(): Unit = native.extern

  @native.name("glfwMakeContextCurrent")
  def glfwMakeContextCurrent(window: native.Ptr[Window]): Unit = native.extern

  @native.name("glfwWindowShouldClose")
  def glfwWindowShouldClose(window: native.Ptr[Window]): native.CInt =
    native.extern

  @native.name("glfwSwapBuffers")
  def glfwSwapBuffers(window: native.Ptr[Window]): Unit = native.extern

  @native.name("glfwGetWindowSize")
  def glfwGetWindowSize(window: native.Ptr[Window],
                        width: native.Ptr[native.CInt],
                        height: native.Ptr[native.CInt]): Unit = native.extern

  @native.name("glfwGetFramebufferSize")
  def glfwGetFramebufferSize(window: native.Ptr[Window],
                             width: native.Ptr[native.CInt],
                             height: native.Ptr[native.CInt]): Unit =
    native.extern

  @native.name("glfwSwapInterval")
  def glfwSwapInterval(interval: native.CInt): Unit = native.extern

  @native.name("glfwCreateWindow")
  def glfwCreateWindow(width: native.CInt,
                       height: native.CInt,
                       title: native.CString,
                       monitor: native.Ptr[Monitor],
                       share: native.Ptr[Window]): native.Ptr[Window] =
    native.extern

  @native.name("glfwGetCursorPos")
  def glfwGetCursorPos(window: native.Ptr[Window],
                       width: native.Ptr[native.CDouble],
                       height: native.Ptr[native.CDouble]): Unit = native.extern

  @native.name("glfwSetCursorPosCallback")
  def glfwSetCursorPosCallback(
      window: native.Ptr[Window],
      callback: native.CFunctionPtr3[native.Ptr[Window],
                                     native.CDouble,
                                     native.CDouble,
                                     Unit]): Unit = native.extern

  @native.name("glfwSetMouseButtonCallback")
  def glfwSetMouseButtonCallback(
      window: native.Ptr[Window],
      callback: native.CFunctionPtr4[native.Ptr[Window],
                                     native.CInt,
                                     native.CInt,
                                     native.CInt,
                                     Unit]): Unit = native.extern

  @native.name("glfwSetScrollCallback")
  def glfwSetScrollCallback(window: native.Ptr[Window],
                            callback: native.CFunctionPtr3[native.Ptr[Window],
                                                           native.CDouble,
                                                           native.CDouble,
                                                           Unit]): Unit =
    native.extern
  @native.name("glfwGetMouseButton")
  def glfwGetMouseButton(window: native.Ptr[Window],
                         flag: native.CInt): native.CInt = native.extern

  @native.name("glfwSetInputMode")
  def glfwSetInputMode(window: native.Ptr[Window],
                       mode: native.CInt,
                       value: native.CInt): Unit = native.extern

}

object Glfw3Constants {
  val GLFW_STICKY_MOUSE_BUTTONS = 0x00033003
  val GLFW_MOUSE_BUTTON_1 = 0
  val GLFW_MOUSE_BUTTON_2 = 1
  val GLFW_RELEASE = 0
  val GLFW_PRESS = 1
}

@native.extern
object GL {

  @native.name("glViewport")
  def glViewport(x: native.CInt,
                 y: native.CInt,
                 width: native.CInt,
                 height: native.CInt): Unit = native.extern

  @native.name("glClearColor")
  def glClearColor(r: native.CFloat,
                   g: native.CFloat,
                   b: native.CFloat,
                   a: native.CFloat): Unit = native.extern

  @native.name("glClear")
  def glClear(flag: native.CInt): Unit = native.extern

}

object GLConstants {
  val DepthBufferBit = 0x00000100
  val StencilBufferBit = 0x00000400
  val ColorBufferBit = 0x00004000
}
