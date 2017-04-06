package org.nspl
import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.nspl.data._
import org.nspl.awtrenderer._

class EventsSpec extends FunSpec with Matchers {

  describe("a") {
    it("a") {

      println("boo")
      val build = Build(group(
        ShapeElem(Shape.rectangle(0, 0, 100, 100), fill = Color.transparent, stroke = Some(Stroke(1f))),
        ShapeElem(Shape.circle(0)),
        FreeLayout
      )) {
        case (Some(old), Scroll(v)) =>
          old.scale(v, v)
        case (Some(old), Click(Point(x, y))) =>
          old.copy(m2 = ShapeElem(Shape.circle(5)).translate(x, y))
      }

      show(build)
    }
  }

}
