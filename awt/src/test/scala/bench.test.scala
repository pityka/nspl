package org.nspl

import org.nspl.data._
import org.nspl.awtrenderer._

class BenchSpec extends munit.FunSuite {

  test("a".ignore) {

    def random = 1 to 10 map (i => scala.util.Random.nextDouble())
    def random2 = 1 to 10 map (i => scala.util.Random.nextGaussian())

    val x = random
    val y = random
    val z = x zip y map (x => x._1 * x._2)
    val z2 = random2
    val z3 = random2
    val idx = 0 until x.size map (_.toDouble)

    def p1 = xyplot(
      (
        indexed(x),
        List(
          point(shapes = Vector(shapeList(1)))
          // line()
        ),
        InLegend("dsf")
      )
    )(
      par(
        ylab = "x",
        xlab = "index",
        main =
          "main\nsdfsd\nasdfsdfd fasd fds fds fds fds fda fdsa fd d ds fds ds df asdf asdf sad sd fsad fsda sda fdsaf "
      )
    ).build

    bench(p1,render=false)

  }

}
