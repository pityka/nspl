package org.nspl

object nanovgutil {

  private var mouseX = 0.0
  private var mouseY = 0.0
  private var dscroll = 0.0
  private var lastScroll = 0.0

  // scala-native throws if local var is reassigned
  private var dragStart: Option[(Point, Bounds)] = None
  private var paintableElem_ : Renderable[_] = _
  private var lastMouse = Point(mouseX, mouseY)
  private var stopLoop = false

  import nanovgrenderer._
  def show[K <: Renderable[K]](elem: Build[K],
                               saveToFile: Option[java.io.File] = None,
                               `yield`: => Unit = ())(
      implicit er: NER[K]
  ) = {
    def paintableElem = paintableElem_.asInstanceOf[K]
    import scala.scalanative.native._
    import scala.scalanative.native
    glfw3.glfwInit()

    glfw3.glfwWindowHint(0x00022006, 1);
    glfw3.glfwWindowHint(0x00022008, 0x00032001);
    glfw3.glfwWindowHint(0x00022002, 3)
    glfw3.glfwWindowHint(0x00022003, 2)

    // msaa 4x:
    // glfw3.glfwWindowHint(0x0002100D, 4)

    val window = glfw3.glfwCreateWindow(600, 600, c"boo", null, null)
    glfw3.glfwMakeContextCurrent(window)
    val vg = nvg.createGL3(Constants.CreateFlags.StencilStrokes)

    nvg.createFont(vg, c"Monospace", c"nanovg/src/main/resources/Go-Mono.ttf")

    val renderingContext = NanovgRC(new NanoVGContext(vg))
    val winWidth = native.stackalloc[native.CInt](1)
    val winHeight = native.stackalloc[native.CInt](1)
    val fbWidth = native.stackalloc[native.CInt](1)
    val fbHeight = native.stackalloc[native.CInt](1)

    glfw3.glfwSetCursorPosCallback(window, native.CFunctionPtr.fromFunction3 {
      (window: native.Ptr[glfw3.Window],
       x: native.CDouble,
       y: native.CDouble) =>
        mouseX = x
        mouseY = y
    })

    glfw3.glfwSetScrollCallback(window, native.CFunctionPtr.fromFunction3 {
      (window: native.Ptr[glfw3.Window],
       x: native.CDouble,
       y: native.CDouble) =>
        dscroll = y - lastScroll
        lastScroll = y

    })

    glfw3.glfwSetInputMode(window, Glfw3Constants.GLFW_STICKY_MOUSE_BUTTONS, 1)

    paintableElem_ = elem.build

    while (glfw3.glfwWindowShouldClose(window) == 0 && !stopLoop) {
      `yield`
      val mouseButtonPressed = glfw3.glfwGetMouseButton(
        window,
        Glfw3Constants.GLFW_MOUSE_BUTTON_1) == Glfw3Constants.GLFW_PRESS

      glfw3.glfwGetWindowSize(window, winWidth, winHeight);
      glfw3.glfwGetFramebufferSize(window, fbWidth, fbHeight);

      val pxRatio = (!fbWidth).toFloat / (!winWidth).toFloat
      nvg.beginFrame(vg, !winWidth, !winHeight, pxRatio);

      GL.glViewport(0, 0, !fbWidth, !fbHeight);
      GL.glClearColor(1f, 1f, 1f, 1f);
      GL.glClear(
        GLConstants.DepthBufferBit | GLConstants.StencilBufferBit | GLConstants.ColorBufferBit);

      val bounds = Bounds(0, 0, !winWidth, !winHeight)
      // Mouse drag
      if (mouseButtonPressed && dragStart.isEmpty) {
        val p = Point(mouseX, mouseY)
        val mapped = mapPoint(p, bounds, paintableElem.bounds)
        dragStart = Some(mapped -> paintableElem.bounds)
        lastMouse = Point(mouseX, mouseY)
      } else if (mouseButtonPressed && dragStart.isDefined) {
        if (lastMouse != Point(mouseX, mouseY)) {
          val p2 =
            mapPoint(Point(mouseX, mouseY), bounds, paintableElem.bounds)
          val p1 =
            mapPoint(dragStart.get._1, dragStart.get._2, paintableElem.bounds)

          lastMouse = Point(mouseX, mouseY)

          paintableElem_ = elem(Some(paintableElem) -> Drag(p1, p2))
          dragStart = Some(p2 -> paintableElem.bounds)
        }
      } else if (!mouseButtonPressed && dragStart.isDefined) {
        dragStart = None
      } else if (dscroll != 0.0) {
        val p =
          mapPoint(Point(mouseX, mouseY), bounds, paintableElem.bounds)
        paintableElem_ = elem(Some(paintableElem) -> Scroll(lastScroll, p))
        dscroll = 0.0
      }

      // Draw elem
      fitToBounds(paintableElem, bounds).render(renderingContext)

      nvg.endFrame(vg);
      saveToFile.foreach { file =>
        stopLoop = true

        native.Zone { implicit z =>
          val bsize = (!fbWidth) * (!fbHeight) * 4
          val buffer = native.alloc[Byte](bsize)
          GL.glReadBuffer(GLConstants.Back)
          GL.glPixelStorei(GLConstants.PackAlignment, 1)
          GL.glReadPixels(0,
                          0,
                          !fbWidth,
                          !fbHeight,
                          GLConstants.RGBA,
                          GLConstants.UnsignedByte,
                          buffer)

          var i = 0
          var j = (!fbHeight) - 1
          var k = 0
          val stride = (!fbWidth) * 4
          while (i < j) {
            val ri = buffer + i * stride
            val rj = buffer + j * stride
            k = 0
            while (k < stride) {
              val t = ri(k)
              ri(k) = rj(k)
              rj(k) = t
              k += 1
            }
            i += 1
            j -= 1
          }

          lodepng.lodepng_encode32_file(native.toCString(file.getAbsolutePath),
                                        buffer,
                                        !fbWidth,
                                        !fbHeight)

        }
      }
      glfw3.glfwSwapBuffers(window)
      glfw3.glfwPollEvents()

    }
    nvg.deleteGL3(vg)
    glfw3.glfwTerminate()
  }
}
