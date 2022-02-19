package org.nspl

import org.nspl.data._
import org.nspl.awtrenderer._

class EventSpec extends munit.FunSuite {

  test("a") {
    {

      println("boo")
      // val build = Build(group(
      //   ShapeElem(Shape.rectangle(0, 0, 100, 100), fill = Color.transparent, stroke = Some(Stroke(1f))),
      //   ShapeElem(Shape.circle(0)),
      //   FreeLayout
      // )) {
      //   case (Some(old), Scroll(v)) =>
      //     old.scale(v, v)
      //   case (Some(old), Click(Point(x, y))) =>
      //     old.copy(m2 = ShapeElem(Shape.circle(5)).translate(x, y))
      // }
      val x = group(
        xyplot(List(1d -> 2d, 3d -> 4d) -> point(color = Color.red))(),
        xyplot(List(1d -> 2d, 3d -> 4d))(par(xlab = "boo")),
        TableLayout(2)
      )
      // println(x.build.m1.bounds)
      // println(x.build.m1.bounds)
      show(x)
    }
  }

}
