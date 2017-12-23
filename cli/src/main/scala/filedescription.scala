package org.nspl.cli

import org.nspl._
import org.nspl.data._
import stringsplit._

object FileDescription {
  def apply(arguments: Seq[Parser.Object]): Seq[FileDescription] =
    arguments.filter(_.name == "file").zipWithIndex.map {
      case (obj @ Parser.Object(_, values), idx) =>
        val min = obj.getFirstInOpt("min").map(_.toInt).getOrElse(1)
        val max = obj.getFirstInOpt("max").map(_.toInt).getOrElse(1000)
        val name =
          obj.getFirstInOpt("name").getOrElse(idx.toString)
        val file = obj.getFirstInOrDefault("file")
        FileDescription(file, min, max, name)
    }
}
case class FileDescription(file: String, min: Int, max: Int, name: String) {
  def openToDataSource = {

    def makeDataSource(iter: Iterator[String],
                       min: Int,
                       max: Int): (DataSource, Update) = {
      val queue = scala.collection.mutable.Queue.empty[VectorRow]
      val dataSource = dataSourceFromRows(queue)

      val update = new Update {
        def update = {
          if (iter.hasNext) {
            val line = iter.next
            val vec = line
              .splitM(Set(' ', '\t'))
              .toVector
              .filterNot(_.isEmpty)
              .map(_.toDouble)
            queue += VectorRow(vec, "")
            if (queue.size > max) {
              queue.dequeue
            }
          }
          ()
        }
      }
      0 until min foreach { _ =>
        update.update
      }
      (dataSource, update)
    }

    val iter = io.Source.fromFile(file).getLines

    (makeDataSource(iter, min = min, max = max), name)

  }
}
