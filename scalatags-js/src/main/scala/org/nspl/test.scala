package org.nspl

import org.nspl.data._
import scalatagrenderer._
import org.scalajs.dom.raw._
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js
import js.annotation.JSExport

@JSExport
object TestSvg {
  @JSExport
  def bind(n: Node): Unit = {
    println("Hi")

    def random = 1 to 1000 map (i => scala.util.Random.nextDouble)
    def random2 = 1 to 1000 map (i => scala.util.Random.nextGaussian)

    val x = random
    val y = random
    val z = x zip y map (x => x._1 * x._2)
    val z2 = random2
    val z3 = random2
    val idx = 0 until x.size map (_.toDouble)

    val p1 = xyplot(indexed(x))(ylab = "x", xlab = "index", main = "main")
    val p2 = xyplot(
      density(x) -> line()
    )(xlab = "x", ylab = "dens.")
    val p3 = xyplot(
      z2 -> z3 -> point(size = 1d, color = Color(200, 200, 200, 255)),
      density2d(z2 zip z3, n = 100, levels = 10)
    )()
    val p4 = xyplot(
      (x zip y zip z map (x => (x._1._1, x._1._2, x._2, x._2 * 10))) -> point(color = HeatMapColors(0.0, 1.0), shapeCol = 3, sizeCol = 5)
    )()

    val p5 = binnedboxplot(x, y, xlab = "PC2", ylab = "PC3")

    val p6 = rasterplot(rasterFromStream(z3.iterator, 30, 30, MinMaxImpl(0.0, 1.0)), xFontSize = 0.5 fts, yFontSize = 0.5 fts)

    val text = fitToWidth(group(
      ShapeElem(Shape.circle(1)),
      TextBox("abc def ghijklmn opqrstvuwxyz"),
      TextBox("abc def ghijklmn opqrstvuwxyz", width = Some(30d)).translate(10, 30),
      TextBox("abc def ghijklmn opqrstvuwxyz", width = Some(30d)).translate(10, 30).rotate(math.Pi / 2, 0d, 0d),
      TextBox("abc def ghijklmn", fontSize = 0.1 fts).rotate(1d),
      TextBox("opqrstvuwxyz", fontSize = 0.1 fts).translate(10, 30),
      TextBox("abc def ghijklmn opqrstvuwxyz", fontSize = 1 fts).translate(100, 30),
      FreeLayout
    ), 200)

    val gallery = group(
      p1,
      p2,
      p3,
      p4,
      p5,
      p6,
      text,
      ColumnLayout(3)
    )

    n.appendChild(renderToScalaTag(gallery, 800).render)

    println("Bye")
  }
}
