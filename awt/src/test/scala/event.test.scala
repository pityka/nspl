package org.nspl
import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.nspl.data._
import org.nspl.awtrenderer._

class EventsSpec extends FunSpec with Matchers {

  describe("a") {
    it("a") {

      println("boo")
      val build = Build {
        case Click(x, y) =>
          println(x, y)
          group(
            ShapeElem(Shape.rectangle(0, 0, 100, 100), fill = Color.transparent, stroke = Some(Stroke(2d))),
            ShapeElem(Shape.circle(10)).translate(x, y),
            FreeLayout
          )
        case e =>
          group(
            ShapeElem(Shape.rectangle(0, 0, 100, 100)),
            ShapeElem(Shape.circle(0)),
            FreeLayout
          )
      }
      show(build)
    }
  }

}
