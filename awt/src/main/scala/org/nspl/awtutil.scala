package org.nspl

import java.awt.Graphics2D
import java.io._

trait JavaAWTUtil {

  implicit class Pimp[K <: Renderable[K]](t: K) {
    def render(ctx: JavaRC)(implicit r: AER[K]) = r.render(ctx, t)
  }

  type AER[T] = Renderer[T, JavaRC]

  implicit def shape2awt(s: Shape): java.awt.Shape = s match {
    case Rectangle(x, y, w, h, tx) => tx.createTransformedShape(new java.awt.geom.Rectangle2D.Double(x, y, w, h))
    case Ellipse(x, y, w, h, tx) => tx.createTransformedShape(new java.awt.geom.Ellipse2D.Double(x, y, w, h))
    case Line(x1, y1, x2, y2) => new java.awt.geom.Line2D.Double(x1, y1, x2, y2)
    case SimplePath(points) => {
      val path = new java.awt.geom.GeneralPath()
      points.foreach { p =>
        path.lineTo(p.x, p.y)
      }
      path.closePath
      path
    }
    case Path(ops) => {
      val path = new java.awt.geom.GeneralPath()
      ops foreach {
        case MoveTo(Point(x, y)) => path.moveTo(x, y)
        case LineTo(Point(x, y)) => path.lineTo(x, y)
        case QuadTo(Point(x2, y2), Point(x1, y1)) => path.quadTo(x1, y1, x2, y2)
        // case CubicTo(Point(x3, y3), Point(x1, y1), Point(x2, y2)) => path.curveTo(x1, y1, x2, y2, x3, y3)
      }
      path.closePath
      path
    }
  }

  implicit def col2col(c: Color): java.awt.Paint =
    new java.awt.Color(c.r, c.g, c.b, c.a)

  implicit def str2str(s: Stroke) = new java.awt.BasicStroke(s.width.toFloat)

  implicit def rec2bounds(r: java.awt.geom.Rectangle2D) =
    Bounds(r.getX, r.getY, r.getWidth, r.getHeight)

  implicit def bounds2rec(r: Bounds) =
    new java.awt.geom.Rectangle2D.Double(r.x, r.y, r.w, r.h)

  implicit def tx2tx(tx: AffineTransform): java.awt.geom.AffineTransform =
    new java.awt.geom.AffineTransform(
      tx.m0, tx.m3, tx.m1, tx.m4, tx.m2, tx.m5
    )

  def show[K <: Renderable[K]](elem: K)(
    implicit
    er: Renderer[K, JavaRC]
  ): Unit = {
    import javax.swing._
    import java.awt.{ Graphics, RenderingHints }
    val frame = new JFrame("");
    frame.setDefaultCloseOperation(javax.swing.WindowConstants.HIDE_ON_CLOSE);
    frame
      .getContentPane()
      .add(new JComponent {
        override def paintComponent(g: Graphics) = {
          super.paintComponent(g)
          val g2 = g.asInstanceOf[Graphics2D]
          g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.
            VALUE_ANTIALIAS_ON)
          g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
          val bounds = getBounds()
          fitToBounds(elem, bounds).render(JavaRC(g2))
        }
      }, java.awt.BorderLayout.CENTER);

    val d = new java.awt.Dimension(elem.bounds.w.toInt, elem.bounds.h.toInt)
    frame.pack();
    frame.setSize(d);
    frame.setVisible(true);
  }

  def savePaint[T](g: Graphics2D)(fun: Graphics2D => T) = {
    val save = g.getPaint
    try { fun(g) } finally { g.setPaint(save) }
  }

  def saveStroke[T](g: Graphics2D)(fun: Graphics2D => T) = {
    val save = g.getStroke
    try { fun(g) } finally { g.setStroke(save) }
  }

  def writeVector[K <: Renderable[K]](
    elem: K,
    os: java.io.OutputStream,
    width: Int = 1000,
    format: String = "pdf"
  )(
    implicit
    er: Renderer[K, JavaRC]
  ) = {
    import de.erichseifert.vectorgraphics2d._
    import util._

    val aspect = elem.bounds.h / elem.bounds.w
    val height = (width * aspect).toInt
    val g2d = format match {
      case "pdf" => new PDFGraphics2D(0, 0, width, height)
      case "svg" => new SVGGraphics2D(0, 0, width, height)
      case "eps" => new EPSGraphics2D(0, 0, width, height)
    }
    val bounds = Bounds(0, 0, width, height)
    fitToBounds(elem, bounds).render(JavaRC(g2d))
    g2d.writeTo(os)

  }

  def write[K <: Renderable[K]](
    elem: K,
    os: java.io.OutputStream,
    width: Int = 1000,
    mimeType: String = "image/png"
  )(
    implicit
    er: Renderer[K, JavaRC]
  ) = {
    mimeType.split("/").last match {
      case "pdf" | "svg" | "eps" => writeVector(elem, os, width, mimeType.split("/").last)
      case _ => writeBitmap(elem, os, width, mimeType)
    }
  }

  def writeBitmap[K <: Renderable[K]](
    elem: K,
    os: java.io.OutputStream,
    width: Int = 1000,
    mimeType: String = "image/png"
  )(
    implicit
    er: Renderer[K, JavaRC]
  ) = {
    import java.awt.image.BufferedImage

    import javax.imageio.ImageIO;
    import javax.imageio.ImageWriter;
    import javax.imageio.stream.ImageOutputStream;
    import java.awt.{ Graphics, RenderingHints }

    val aspect = elem.bounds.h / elem.bounds.w
    val height = (width * aspect).toInt

    val bimage = new BufferedImage(
      width, height, BufferedImage.TYPE_INT_ARGB
    );

    val g2d = bimage.createGraphics();

    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

    val bounds = Bounds(0, 0, width, height)
    fitToBounds(elem, bounds).render(JavaRC(g2d))

    {
      val imageWriter = ImageIO.getImageWritersByMIMEType(mimeType).next

      val ios = ImageIO.createImageOutputStream(os);
      imageWriter.setOutput(ios)
      try {
        imageWriter.write(bimage);
      } finally {
        ios.close();
      }

    }

  }

  private def writeBinaryToFile(f: File, data: Array[Byte]): Unit = {
    val os = new BufferedOutputStream(new FileOutputStream(f))
    try {
      os.write(data)
    } finally {
      os.close
    }
  }

  def renderToByteArray[K <: Renderable[K]](
    elem: K,
    width: Int = 1000,
    mimeType: String = "image/png"
  )(
    implicit
    er: Renderer[K, JavaRC]
  ): Array[Byte] = {
    val bs = new java.io.ByteArrayOutputStream()
    write(elem, bs, width, mimeType);
    bs.toByteArray
  }

  def renderToFile[K <: Renderable[K]](
    elem: K,
    width: Int = 1000,
    mimeType: String = "image/png"
  )(
    implicit
    er: Renderer[K, JavaRC]
  ) = {
    val f = java.io.File.createTempFile("nspl", "." + mimeType.split("/").last)
    val os = new BufferedOutputStream(new FileOutputStream(f))
    try {
      write(elem, os, width, mimeType)
    } finally {
      os.close
    }
    f
  }

  def renderToFile[K <: Renderable[K]](
    f: File,
    elem: K,
    width: Int,
    mimeType: String
  )(
    implicit
    er: Renderer[K, JavaRC]
  ) = {
    val os = new BufferedOutputStream(new FileOutputStream(f))
    try {
      write(elem, os, width, mimeType)
    } finally {
      os.close
    }
    f
  }

  def pdfToFile[K <: Renderable[K]](
    f: File,
    elem: K,
    width: Int
  )(
    implicit
    er: Renderer[K, JavaRC]
  ) = {
    renderToFile(elem, width, "application/pdf")
    f
  }

  def pdfToFile[K <: Renderable[K]](
    elem: K,
    width: Int = 1000
  )(
    implicit
    er: Renderer[K, JavaRC]
  ) = {
    val f = java.io.File.createTempFile("nspl", ".pdf")
    renderToFile(f, elem, width, "application/pdf")
    f
  }

  def pngToFile[K <: Renderable[K]](
    f: File,
    elem: K,
    width: Int
  )(
    implicit
    er: Renderer[K, JavaRC]
  ) = {
    renderToFile(f, elem, width, "image/png")
    f
  }

  def pngToFile[K <: Renderable[K]](
    elem: K,
    width: Int = 1000
  )(
    implicit
    er: Renderer[K, JavaRC]
  ) = {
    renderToFile(elem, width, "image/png")
  }

}
