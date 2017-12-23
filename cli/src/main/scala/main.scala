package org.nspl.cli

import org.nspl._
import org.nspl.data._
import stringsplit._

trait Update {
  def update: Unit
}

/** Command line interface
  *
  * e.g. nspl-nanovg-native-cli-out file[<(printf "1 2\n3 4\n") --min 2] xy[data[point[] --file 0]]
  */
object PlotCli extends App {

  val arguments = args.mkString(" ")
  val parsed = Parser.apply(arguments).get.value

  val files = FileDescription(parsed)
  println("files: \n" + files.mkString("\t", "\n", ""))
  val dataSources = files.map(_.openToDataSource)
  val dataSourceMap = dataSources.map(x => x._2 -> x._1._1).toMap
  val plots = XYPlotDescription(parsed, dataSourceMap)

  println(s"${plots.size} plots.")
  val figure = sequence(plots, TableLayout(3))

  val saveToFile = SaveToFile(parsed)
  import nanovgrenderer._
  nanovgutil.show(figure, `yield` = {
    dataSources.map(_._1._2).foreach(_.update)
  }, saveToFile = saveToFile)

}
