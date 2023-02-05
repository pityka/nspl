import org.nspl._
import org.nspl.data._
import canvasrenderer._
import org.scalajs.dom._

import scala.scalajs.js
import js.annotation._

@JSExportTopLevel("nsplcanvastest")
object nsplcanvastest {
  @JSExport
  def bind(n: Node): Unit = {
    println("Hi")

    def random = 1 to 1000 map (_ => scala.util.Random.nextDouble())
    def random2 = 1 to 1000 map (_ => scala.util.Random.nextGaussian())

    val x = random
    val y = random
    val z = x zip y map (x => x._1 * x._2)
    val z2 = random2
    val z3 = random2

    val p1 = xyplot(
      (
        indexed(x),
        List(
          point(shapes = Vector(shapeList(1)))
          // line()
        ),
        InLegend("dsf")
      )
    )(
      par.
        ylab ("x")
        .xlab ("index")
        .main(
          "main\nsdfsd\nasdfsdfd fasd fds fds fds fds fda fdsa fd d ds fds ds df asdf asdf sad sd fsad fsda sda fdsaf ")
      )
    

    val p2 = xyplot(
      density(x) -> line()
    )(par.xlab("x").ylab("dens"))
    val p3 = xyplot(
      z2 -> z3 -> point(size = 1d, color = Color(200, 200, 200, 255)),
      density2d(z2 zip z3, n = 100, levels = 10)
    )()
    val p4 = xyplot(
      (x zip y zip z map (x => (x._1._1, x._1._2, x._2, x._2 * 10))) -> point(
        color = HeatMapColors(0.0, 1.0),
        shapeCol = 3,
        sizeCol = 5
      )
    )()

    val p5 = binnedboxplot(x, y)(par.xlab("PC2").ylab ("PC3"))

    val p6 = rasterplot(
      rasterFromStream(z3.iterator, 30, 30, MinMaxImpl(0.0, 1.0))
    )(par.xLabFontSize(0.5 fts).yLabFontSize(0.5 fts))

    val text: Elems7[
      ShapeElem,
      TextBox,
      TextBox,
      TextBox,
      TextBox,
      TextBox,
      TextBox
    ] = fitToWidth(
      group(
        ShapeElem(Shape.circle(1)),
        TextBox("abc def ghijklmn opqrstvuwxyz"),
        TextBox("abc def ghijklmn opqrstvuwxyz", width = Some(30d))
          .translate(10, 30),
        TextBox("abc def ghijklmn opqrstvuwxyz", width = Some(30d))
          .translate(10, 30)
          .rotate(math.Pi / 2, 0d, 0d),
        TextBox("abc def ghijklmn", fontSize = 0.1 fts).rotate(1d),
        TextBox("opqrstvuwxyz", fontSize = 0.1 fts).translate(10, 30),
        TextBox("abc def ghijklmn opqrstvuwxyz", fontSize = 1 fts)
          .translate(100, 30),
        FreeLayout
      ),
      200
    )

    val cubeVertex: DataSource = List(
      (0d, 0d, 0d),
      (100d, 0d, 0d),
      (100d, 100d, 0d),
      (100d, 100d, 0d),
      (100d, 0d, 100d)
    )

    val cube: DataSource =
      List(
        (0d, 0d, 0d),
        (100d, 0d, 0d),
        (100d, 0d, 0d),
        (100d, 100d, 0d),
        (100d, 100d, 0d),
        (0d, 100d, 0d),
        (0d, 100d, 0d),
        (0d, 0d, 0d),
        (0d, 0d, 100d),
        (100d, 0d, 100d),
        (100d, 0d, 100d),
        (100d, 100d, 100d),
        (100d, 100d, 100d),
        (0d, 100d, 100d),
        (0d, 100d, 100d),
        (0d, 0d, 100d),
        (0d, 0d, 0d),
        (0d, 0d, 100d),
        (100d, 0d, 0d),
        (100d, 0d, 100d),
        (100d, 100d, 0d),
        (100d, 100d, 100d),
        (0d, 100d, 0d),
        (0d, 100d, 100d)
      ).grouped(2)
        .toList
        .map(v => (v(0)._1, v(0)._2, v(0)._3, v(1)._1, v(1)._2, v(1)._3))

    val xyzp: Build[Elems2[XYZPlotArea, Legend]] = xyzplot(
      (cube, List(lineSegment3D()), NotInLegend),
      (cubeVertex, List(point3D()), NotInLegend)
    )()

    val gallery = group(
      xyzp,
      p1,
      p2,
      p2,
      p3,
      p4,
      p5,
      p6,
      text,
      VerticalStack(Align.Anchor)
    )

    val (canv, _) = render(gallery, 800, 800, println)

    n.appendChild(canv)

    println("Bye")
  }
}
