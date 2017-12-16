package org.nspl

object nanovgutil {
  import nanovgrenderer._
  def show[K <: Renderable[K]](elem: Build[K])(
      implicit er: NER[K]
  ) = {
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
    while (glfw3.glfwWindowShouldClose(window) == 0) {

         
          

      glfw3.glfwGetWindowSize(window, winWidth, winHeight);
      glfw3.glfwGetFramebufferSize(window, fbWidth, fbHeight);
      // Calculate pixel ration for hi-dpi devices.
      val pxRatio = (!fbWidth).toFloat / (!winWidth).toFloat
      nvg.beginFrame(vg, !winWidth, !winHeight, pxRatio);
      // Update and render
      GL.glViewport(0, 0, !fbWidth, !fbHeight);
      GL.glClearColor(1f, 1f, 1f, 1f);
      GL.glClear(
        GLConstants.DepthBufferBit | GLConstants.StencilBufferBit | GLConstants.ColorBufferBit);

      val bounds = Bounds(0, 0, !winWidth, !winHeight)
      val paintableElem = elem.build
      fitToBounds(paintableElem, bounds).render(renderingContext)
      
      nvg.endFrame(vg);
      glfw3.glfwSwapBuffers(window)
      glfw3.glfwPollEvents()
    }
    nvg.deleteGL3(vg)
    glfw3.glfwTerminate()
  }
}
