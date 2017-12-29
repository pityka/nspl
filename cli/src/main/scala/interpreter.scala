package org.nspl.cli

import org.nspl._
import org.nspl.data._

object DataRenderer {

  def parseStroke(obj: Parser.Object): Stroke =
    Stroke(
      width = obj.getFirstInOrDefaultOpt("width").map(_.toDouble).getOrElse(1d),
      cap = obj
        .getFirstInOpt("cap")
        .map(_ match {
          case "square" => CapSquare
          case "butt"   => CapButt
          case "round"  => CapRound
        })
        .getOrElse(CapSquare)
    )

  def parseColormap(obj: Parser.Object): Colormap = {
    obj.name match {
      case "color" =>
        parseColor(obj)
      case "colorlist" =>
        ManualColor(
          obj.arguments
            .get("_default_")
            .getOrElse(Nil)
            .collect {
              case obj @ Parser.Object("color", _) => obj
            }
            .map(parseColor)
            .zipWithIndex
            .map { case (color, idx) => idx.toDouble -> color }
            .toMap)
    }
  }

  def parseColor(obj: Parser.Object): Color =
    obj.arguments.get("_default_") match {
      case Some(Parser.Value("black") +: xs) => Color.black
      case Some(Parser.Value("red") +: xs)   => Color.red
      case Some(Parser.Value("green") +: xs) => Color.green
      case Some(Parser.Value("blue") +: xs)  => Color.blue
      case Some(Parser.Value("gray1") +: xs) => Color.gray1
      case Some(Parser.Value("gray2") +: xs) => Color.gray2
      case Some(Parser.Value("gray3") +: xs) => Color.gray3
      case Some(Parser.Value("gray4") +: xs) => Color.gray4
      case None =>
        Color(
          r = obj.getFirstInOpt("r").map(_.toInt).getOrElse(0),
          g = obj.getFirstInOpt("g").map(_.toInt).getOrElse(0),
          b = obj.getFirstInOpt("b").map(_.toInt).getOrElse(0)
        )
    }

  def parsePointRenderer(obj: Parser.Object): DataRenderer = {
    point(
      xCol = obj.getFirstInOpt("xCol").map(_.toInt).getOrElse(0),
      yCol = obj.getFirstInOpt("yCol").map(_.toInt).getOrElse(1),
      colorCol = obj.getFirstInOpt("colorCol").map(_.toInt).getOrElse(2),
      sizeCol = obj.getFirstInOpt("sizeCol").map(_.toInt).getOrElse(3),
      shapeCol = obj.getFirstInOpt("shapeCol").map(_.toInt).getOrElse(4),
      errorTopCol = obj.getFirstInOpt("errorTopCol").map(_.toInt).getOrElse(5),
      errorBottomCol =
        obj.getFirstInOpt("errorBottomCol").map(_.toInt).getOrElse(6),
      size = obj.getFirstInOpt("size").map(_.toDouble).getOrElse(3d),
      color =
        obj.getObject("color").map(parseColormap).getOrElse(DiscreteColors(14)),
      shapes = shapeList,
      pointSizeIsInDataSpaceUnits =
        obj.getBoolean("pointSizeIsInDataSpaceUnits").getOrElse(false),
      valueText = obj.getBoolean("valueText").getOrElse(false),
      labelText = obj.getBoolean("labelText").getOrElse(false),
      labelFontSize = obj
        .getFirstInOpt("labelFontSize")
        .map(_.toDouble.fts)
        .getOrElse(0.4 fts),
      labelColor =
        obj.getObject("labelColor").map(parseColor).getOrElse(Color.black),
      errorBarStroke =
        obj.getObject("errorBarStroke").map(parseStroke).getOrElse(Stroke(1d))
    )
  }

  def parseLineRenderer(obj: Parser.Object): DataRenderer = {

    line(
      xCol = obj.getFirstInOpt("xCol").map(_.toInt).getOrElse(0),
      yCol = obj.getFirstInOpt("yCol").map(_.toInt).getOrElse(1),
      colorCol = obj.getFirstInOpt("colorCol").map(_.toInt).getOrElse(2),
      color = obj.getObject("color").map(parseColormap).getOrElse(Color.black),
      stroke = obj.getObject("stroke").map(parseStroke).getOrElse(Stroke(1d))
    )
  }

  def parseData(data: Parser.Object, dataSources: Map[String, DataSource])
    : (DataSource, List[DataRenderer], LegendConfig) = {
    val name: String = data.getFirstInOpt("file").getOrElse("0")
    val dataSource = dataSources
      .get(name)
      .getOrElse(
        throw new RuntimeException(
          s"data source with this name not found: $name"))
    val renderers = data.arguments
      .get("_default_")
      .toList
      .flatten
      .collect {
        case point @ Parser.Object("point", _) => parsePointRenderer(point)
        case line @ Parser.Object("line", _)   => parseLineRenderer(line)
      }
    val r = if (renderers.isEmpty) {
      println("No renderer configured. Add default point().")
      List(point())
    } else renderers
    (dataSource, r, NotInLegend)
  }

  def apply(xy: Parser.Object, dataSources: Map[String, DataSource])
    : Seq[(DataSource, List[DataRenderer], LegendConfig)] =
    xy.arguments
      .get("_default_")
      .toList
      .flatten
      .collect {
        case data @ Parser.Object("data", _) => parseData(data, dataSources)
      }
}

object XYPlotDescription {

  def parseDoublePair(obj: Parser.Object): (Double, Double) = {
    (obj.getFirstIn("1").toDouble, obj.getFirstIn("2").toDouble)
  }

  def apply(arguments: Seq[Parser.Object],
            dataSources: Map[String, DataSource]) =
    arguments
      .filter(_.name == "xy")
      .zipWithIndex
      .map {
        case (obj @ Parser.Object(_, values), idx) =>
          val data = DataRenderer(obj, dataSources)
          if (data.isEmpty) {
            println("Skip empty plot")
            None
          } else {
            Some(
              xyplot(data: _*)(
                xlog = obj.getBoolean("xlog").getOrElse(false),
                ylog = obj.getBoolean("ylog").getOrElse(false),
                main = obj.getFirstInOpt("main").getOrElse(""),
                xlab = obj.getFirstInOpt("xlab").getOrElse(""),
                ylab = obj.getFirstInOpt("ylab").getOrElse(""),
                xlim = obj.getObject("xlim").map(parseDoublePair),
                ylim = obj.getObject("ylim").map(parseDoublePair),
                draw1Line = obj.getBoolean("draw1Line").getOrElse(false),
                xLabFontSize = obj
                  .getFirstInOpt("xLabFontSize")
                  .map(_.toDouble.fts)
                  .getOrElse(1d fts),
                yLabFontSize = obj
                  .getFirstInOpt("yLabFontSize")
                  .map(_.toDouble.fts)
                  .getOrElse(1d fts),
                mainFontSize = obj
                  .getFirstInOpt("mainFontSize")
                  .map(_.toDouble.fts)
                  .getOrElse(1d fts),
                xNumTicks =
                  obj.getFirstInOpt("xNumTicks").map(_.toInt).getOrElse(4),
                yNumTicks =
                  obj.getFirstInOpt("yNumTicks").map(_.toInt).getOrElse(4),
                xAxisMargin = obj
                  .getFirstInOpt("xAxisMargin")
                  .map(_.toDouble)
                  .getOrElse(0.05),
                yAxisMargin = obj
                  .getFirstInOpt("yAxisMargin")
                  .map(_.toDouble)
                  .getOrElse(0.05),
                xgrid = obj.getBoolean("xgrid").getOrElse(false),
                ygrid = obj.getBoolean("ygrid").getOrElse(false),
                xWidth = obj
                  .getFirstInOpt("xWidth")
                  .map(_.toDouble.fts)
                  .getOrElse(20 fts),
                yHeight = obj
                  .getFirstInOpt("yHeight")
                  .map(_.toDouble.fts)
                  .getOrElse(20 fts),
                frame = obj.getBoolean("frame").getOrElse(true),
                xLabelRotation = obj
                  .getFirstInOpt("xLabelRotation")
                  .map(_.toDouble)
                  .getOrElse(0.00),
                yLabelRotation = obj
                  .getFirstInOpt("yLabelRotation")
                  .map(_.toDouble)
                  .getOrElse(0.00),
                origin = obj.getObject("origin").map(parseDoublePair)
              ))
          }

      }
      .filter(_.isDefined)
      .map(_.get)
}

object SaveToFile {
  def apply(arguments: Seq[Parser.Object]): Option[java.io.File] =
    arguments
      .find(_.name == "save")
      .map {
        case obj @ Parser.Object(_, values) =>
          new java.io.File(obj.getFirstInOrDefault("file"))
      }
}

object Size {
  def apply(arguments: Seq[Parser.Object]): Option[(Int, Int)] =
    arguments
      .find(_.name == "size")
      .flatMap {
        case obj @ Parser.Object(_, values) =>
          val w = obj.getFirstInOpt("width").map(_.toInt)
          val h = obj.getFirstInOpt("height").map(_.toInt)
          for { w <- w; h <- h } yield (w, h)
      }
}
