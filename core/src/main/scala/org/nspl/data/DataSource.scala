package org.nspl.data;

trait MinMax {
  def min: Double
  def max: Double
}

trait DataSource {

  def iterator: Iterator[Row]
  def columnMinMax(i: Int): MinMax
  def dimension: Int

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

}

case class MinMaxImpl(min: Double, max: Double) extends MinMax
