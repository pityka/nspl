package mdoc.docs

import java.nio.file.Files
import java.nio.file.Paths
import mdoc._
import scala.meta.inputs.Position
import org.nspl.awtrenderer._

class BytesModifier extends PostModifier {
  val name = "bytes"
  def process(ctx: PostModifierContext): String = {
    val relpath = Paths.get(ctx.info)
    val out = ctx.outputFile.toNIO.getParent.resolve(relpath)
    ctx.lastValue match {
      case data: Array[_] if data.length > 0 && data.head.isInstanceOf[Byte] =>
        Files.createDirectories(out.getParent)
        Files.write(out, data.asInstanceOf[Array[Byte]])
        s"""
```scala
${ctx.originalCode.text}
```
The above code block produces this plot: ![](${relpath})"""
      case _ =>
        val (pos, obtained) = ctx.variables.lastOption match {
          case Some(variable) =>
            val prettyObtained =
              s"${variable.staticType} = ${variable.runtimeValue}"
            (variable.pos, prettyObtained)
          case None =>
            (Position.Range(ctx.originalCode, 0, 0), "nothing")
        }
        ctx.reporter.error(
          pos,
          s"""type mismatch:
  expected: com.cibo.evilplot.geometry.Drawable
  obtained: $obtained"""
        )
        ""
    }
  }
}