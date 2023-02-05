package org.nspl

import org.nspl.awtrenderer._

class BenchSpec extends munit.FunSuite {

  test("a".ignore) {

    def random = 1 to 10 map (_ => scala.util.Random.nextDouble())

    val x = random

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
      par.ylab("x").xlab("index").main("main\nsdfsd\nasdfsdfd fasd fds fds fds fds fda fdsa fd d ds fds ds df asdf asdf sad sd fsad fsda sda fdsaf ")
        
    ).build

    bench(p1,render=false)

  }

}
