package org.nspl

case class NanovgRC(ctx: NanoVGContext) extends RenderingContext

object nanovgrenderer {

  implicit val defaultAWTFont: FontConfiguration = Monospace

  type NER[T] = Renderer[T, NanovgRC]

  implicit class Pimp[K <: Renderable[K]](t: K) {
    def render(ctx: NanovgRC)(implicit r: NER[K]) = r.render(ctx, t)
  }

  implicit class PimpedTx(tx: AffineTransform) {
    def applyTo(ctx: NanovgRC) = {
      nvg.transform(ctx.ctx.ctx,
                    tx.m0.toFloat,
                    tx.m3.toFloat,
                    tx.m1.toFloat,
                    tx.m4.toFloat,
                    tx.m2.toFloat,
                    tx.m5.toFloat)
    }
  }

  implicit class PimpedColor(c: Color) {
    import scalanative.native._
    def makeNvg = nvg.rgba(c.r.toUByte, c.g.toUByte, c.b.toUByte, c.a.toUByte)
  }

  def makePath(sh: Shape, ctx: NanovgRC) = sh match {
    case Rectangle(x, y, w, h, tx) =>
      tx.applyTo(ctx)
      nvg.beginPath(ctx.ctx.ctx)
      nvg.rect(ctx.ctx.ctx, x.toFloat, y.toFloat, w.toFloat, h.toFloat)

    case Ellipse(x, y, w, h, tx) =>
      tx.applyTo(ctx)
      nvg.beginPath(ctx.ctx.ctx)
      pathEllipse(ctx, x, y, w, h)

    case Line(x1, y1, x2, y2) => 
      nvg.beginPath(ctx.ctx.ctx)
        nvg.moveTo(ctx.ctx.ctx,x1.toFloat,y1.toFloat)
        nvg.lineTo(ctx.ctx.ctx, x2.toFloat, y2.toFloat)

    case SimplePath(points) =>
      nvg.beginPath(ctx.ctx.ctx)
      if (points.size >= 2){
  nvg.moveTo(ctx.ctx.ctx,points.head.x.toFloat,points.head.x.toFloat)
      points.drop(1).foreach { p =>
        nvg.lineTo(ctx.ctx.ctx, p.x.toFloat, p.y.toFloat)
      }
      }

    case Path(ops) =>
      nvg.beginPath(ctx.ctx.ctx)
      ops foreach {
        case MoveTo(Point(x, y)) =>
          nvg.moveTo(ctx.ctx.ctx, x.toFloat, y.toFloat)
        case LineTo(Point(x, y)) =>
          nvg.lineTo(ctx.ctx.ctx, x.toFloat, y.toFloat)
        case QuadTo(Point(x2, y2), Point(x1, y1)) =>
          nvg.quadTo(ctx.ctx.ctx,
                     x1.toFloat,
                     y1.toFloat,
                     x2.toFloat,
                     y2.toFloat)
        // case CubicTo(Point(x3, y3), Point(x1, y1), Point(x2, y2)) => path.curveTo(x1, y1, x2, y2, x3, y3)
      }
  }

  implicit val shapeRenderer = new NER[ShapeElem] {
    def render(ctx: NanovgRC, elem: ShapeElem): Unit = {
      AffineTransform.identity.applyTo(ctx)
      nvg.save(ctx.ctx.ctx)
      if (elem.fill.a > 0.0) {
        nvg.fillColor(ctx.ctx.ctx, elem.fill.makeNvg)
        makePath(elem.shape, ctx)
        nvg.fill(ctx.ctx.ctx)
      }
      if (elem.stroke.isDefined) {
        nvg.strokeWidth(ctx.ctx.ctx, elem.stroke.get.width.toFloat)
        nvg.strokeColor(ctx.ctx.ctx, elem.strokeColor.makeNvg)
        makePath(elem.shape, ctx)
        nvg.stroke(ctx.ctx.ctx)
      }
      nvg.restore(ctx.ctx.ctx)
    }
  }

  def pathEllipse(ctx: NanovgRC,
                  x: Double,
                  y: Double,
                  width: Double,
                  height: Double) = {
    val centerX = x + .5 * width
    val centerY = y + .5 * height
    val radiusX = width * .5
    val radiusY = height * .5
    nvg.ellipse(ctx.ctx.ctx,
                centerX.toFloat,
                centerY.toFloat,
                radiusX.toFloat,
                radiusY.toFloat)
  }

  implicit val textRenderer = new NER[TextBox] {
    import scalanative.native._
    import scalanative.native
    def render(ctx: NanovgRC, elem: TextBox): Unit = {
      nvg.save(ctx.ctx.ctx)     
      
      nvg.beginPath(ctx.ctx.ctx)
      if (elem.text.size > 0) {
        
        nvg.fillColor(ctx.ctx.ctx, elem.color.makeNvg)

        elem.font match {
          case Monospace =>
            nvg.fontFace(ctx.ctx.ctx, c"Monospace")
            nvg.fontSize(ctx.ctx.ctx, Monospace.size)
          case NamedFont(name, size) =>   
            native.Zone { implicit z =>         
              nvg.fontFace(ctx.ctx.ctx,native.toCString(name))
            }
            nvg.fontSize(ctx.ctx.ctx,size)
        }

        elem.layout.lines.foreach {
          case (line, lineTx) =>
            val tx = elem.txLoc.concat(lineTx)
            tx.applyTo(ctx)
            native.Zone { implicit z =>
              nvg.text(ctx.ctx.ctx, 0, 0, native.toCString(line), null)
            }

        }
        
      }
      nvg.restore(ctx.ctx.ctx)
      
    }
  }
}
