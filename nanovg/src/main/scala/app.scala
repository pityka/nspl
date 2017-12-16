package org.nspl

object NanoVgWindow extends App {

  import nanovgrenderer._
  val elem = xyplot(List(1d -> 2d, 3d -> 4d) -> line(color=Color.black))()

  nanovgutil.show(group(elem,elem, VerticalStack()))

}
