package org.nspl.cli

import org.nspl._
import org.nspl.data._

object DataRenderer {

  def parsePointRenderer(obj: Parser.Object): DataRenderer = {
    point(
      xCol = obj.getFirstInOpt("xCol").map(_.toInt).getOrElse(0),
      yCol = obj.getFirstInOpt("yCol").map(_.toInt).getOrElse(1)
      //   colorCol: Int = 2,
      //   sizeCol: Int = 3,
      //   shapeCol: Int = 4,
      //   errorTopCol: Int = 5,
      //   errorBottomCol: Int = 6,
      //   size: Double = 3.0,
      //   color: Colormap = DiscreteColors(14),
      //   shapes: Vector[Shape] = shapeList,
      //   pointSizeIsInDataSpaceUnits: Boolean = false,
      //   valueText: Boolean = false,
      //   labelText: Boolean = false,
      //   labelFontSize: RelFontSize = 0.4 fts,
      //   labelColor: Color = Color.black,
      //   errorBarStroke: Stroke = Stroke(1d)
    )
  }

  def parseData(data: Parser.Object, dataSources: Map[String, DataSource])
    : (DataSource, List[DataRenderer], LegendConfig) = {
    val name: String = data.getFirstIn("file")
    val dataSource = dataSources
      .get(name)
      .getOrElse(
        throw new RuntimeException(
          "data source with this name not found: $name"))
    val renderers = data.arguments
      .get("_default_")
      .toList
      .flatten
      .collect {
        case point @ Parser.Object("point", _) => parsePointRenderer(point)
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
            Some(xyplot(data: _*)())
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
