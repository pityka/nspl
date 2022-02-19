package org.nspl.data;

trait MinMax {
  def min: Double
  def max: Double
}

/** Basic trait of all data sources.
  *
  * Represents an iterator of fixed size records (rows).
  */
trait DataSource {

  def iterator: Iterator[Row]
  def columnMinMax(i: Int): Option[MinMax]
  def dimension: Int
  def columnNames: Seq[String] = 0 until dimension map (_ => "")

}

trait DataSourceWithQuantiles extends DataSource {
  def quantilesOfColumn(i: Int, qs: Vector[Double]): Vector[Double]
}

trait Row {
  def apply(i: Int): Double
  def allColumns: Seq[Double]
  def dimension: Int
  def label: String
}

case class VectorRow(values: Vector[Double], label: String) extends Row {

  def apply(i: Int) = values(i)
  def allColumns = values
  def dimension = values.length

  def withLabel(s: String) = copy(label = s)

}

object VectorRow {
  def apply(values: Double*): VectorRow = VectorRow(values.toVector, "")
  def apply(label: String, values: Double*): VectorRow =
    VectorRow(values.toVector, label)

}

case class MinMaxImpl(min: Double, max: Double) extends MinMax
