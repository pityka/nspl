import org.nspl._
import canvasrenderer._
import org.scalajs.dom.document

import scala.scalajs.js.annotation._

/** Interactive canvas demo. The DOM scaffolding (title, instructions, slots,
  * log) lives in `canvas/index.html`; this object only attaches the rendered
  * canvases to the placeholder elements and routes interaction callbacks to
  * the `#event-log` `<pre>`. Build with `sbt canvas/Test/fastLinkJS` and
  * open `canvas/index.html`.
  */
@JSExportTopLevel("nsplcanvastest")
object nsplcanvastest {

  private def buildScatter() = {
    val rng = new scala.util.Random(42)
    val data = (1 to 200).map { i =>
      val cluster = i / 50
      (
        rng.nextGaussian() + cluster * 3d,
        rng.nextGaussian() + cluster * 2d,
        cluster.toDouble
      )
    }
    xyplot(
      data -> point(
        color = DiscreteColors(8),
        size = 5d,
        noIdentifier = false
      )
    )(par.xlab("x").ylab("y"))
  }

  private def buildLine() = xyplot(
    (1 to 50).map(i => (i.toDouble, math.sin(i * 0.2))) -> line()
  )(par.xlab("i").ylab("sin"))

  private def fmtId(id: Identifier): String = id match {
    case DataRowIdx(ext, ds, row) =>
      s"DataRow(externalDS=$ext, ds=$ds, row=$row)"
    case TextBoxIdentifier(label, idx) =>
      s"TextBox(label=$label, index=$idx)"
    case PlotAreaIdentifier(_, _, _) => "PlotArea"
    case EmptyIdentifier              => "Empty"
    case other                        => other.toString
  }

  @JSExport
  def bind(): Unit = {
    val scatterSlot = document.getElementById("scatter-plot")
    val lineSlot = document.getElementById("line-plot")
    val log = document.getElementById("event-log")

    if (scatterSlot == null || lineSlot == null || log == null) {
      // index.html shape changed and the slots aren't where we expect.
      // Surface the mismatch loudly rather than silently doing nothing.
      throw new RuntimeException(
        "nsplcanvastest: expected DOM elements #scatter-plot, #line-plot, #event-log"
      )
    }

    def emit(line: String): Unit = {
      val p = document.createElement("div")
      p.textContent = line
      log.insertBefore(p, log.firstChild)
      while (log.childNodes.length > 200) log.removeChild(log.lastChild)
    }

    val (scatterCanvas, _) = render(
      buildScatter(),
      width = 600,
      height = 400,
      onShapeClick = Some { (id, _) => emit(s"click  ${fmtId(id)}") },
      onHover = Some { (id, _) => emit(s"hover  ${fmtId(id)}") },
      onUnhover = Some { (id, _) => emit(s"unhover ${fmtId(id)}") },
      onSelection = Some { ids =>
        val sample = ids.take(8).map(fmtId).mkString(", ")
        val tail = if (ids.size > 8) " …" else ""
        emit(s"select ${ids.size} shapes  → $sample$tail")
      }
    )
    val (lineCanvas, _) = render(
      buildLine(),
      width = 600,
      height = 200,
      click = id => emit(s"plotMouseDown ${fmtId(id)}")
    )

    scatterSlot.appendChild(scatterCanvas)
    lineSlot.appendChild(lineCanvas)
    emit("ready")
  }
}
