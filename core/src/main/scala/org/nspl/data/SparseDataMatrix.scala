package org.nspl.data

object SparseDataMatrix {

  /** Sorts `elems` into row-major order and constructs a [[SparseDataMatrix]].
    * The first `Int` is the row index, the second is the column index.
    */
  def fromUnsorted(
      elems: Vector[(Int, Int, Double)],
      numCols: Int,
      numRows: Int,
      missingValue: Double
  ): SparseDataMatrix =
    SparseDataMatrix(
      elems.sortBy(_._2).sortBy(_._1),
      numCols,
      numRows,
      missingValue
    )
}

/** A `DataSource` view over a sparse matrix. Missing cells return
  * `missingValue`. Yields one [[Row]] per (row, col) pair in row-major order;
  * each row has three columns: (colIndex, rowIndex, value).
  *
  * `elemsSorted` MUST be sorted in row-major order
  * (`(rowIndex * numCols + colIndex)` ascending). Use
  * [[SparseDataMatrix.fromUnsorted]] if not already sorted.
  */
case class SparseDataMatrix(
    elemsSorted: Vector[(Int, Int, Double)],
    numCols: Int,
    numRows: Int,
    missingValue: Double
) extends DataSource {

  val dimension = 3
  private val n = numCols.toLong * numRows.toLong

  def iterator: Iterator[Row] = new Iterator[Row] {
    private var k = 0L
    private var ptr = 0
    private val sortedSize = elemsSorted.size

    def hasNext: Boolean = k < n

    def next(): Row = {
      if (!hasNext) throw new NoSuchElementException("SparseDataMatrix.next")
      val iRow = (k / numCols).toInt
      val jCol = (k % numCols).toInt

      while (ptr < sortedSize && {
               val t = elemsSorted(ptr)
               t._1.toLong * numCols + t._2 < k
             }) ptr += 1

      val value =
        if (ptr < sortedSize) {
          val t = elemsSorted(ptr)
          if (t._1.toLong * numCols + t._2 == k) t._3 else missingValue
        } else missingValue

      k += 1L

      new Row {
        def apply(l: Int): Double =
          if (l == 0) jCol.toDouble
          else if (l == 1) iRow.toDouble
          else value
        def allColumns: Seq[Double] =
          Vector(jCol.toDouble, iRow.toDouble, value)
        def dimension: Int = 3
        def label: String = ""
        override def toString: String = value.toString
      }
    }
  }

  def columnMinMax(i: Int): Option[MinMax] = Some {
    if (i == 0)
      MinMaxImpl(0.0, (numCols - 1).toDouble)
    else if (i == 1)
      MinMaxImpl(0.0, (numRows - 1).toDouble)
    else {
      val (minV, maxV) =
        if (elemsSorted.isEmpty) (missingValue, missingValue)
        else {
          val values = elemsSorted.iterator.map(_._3)
          var lo = Double.PositiveInfinity
          var hi = Double.NegativeInfinity
          while (values.hasNext) {
            val v = values.next()
            if (v < lo) lo = v
            if (v > hi) hi = v
          }
          (lo, hi)
        }
      // include the missing-value sentinel only if there is at least one
      // missing cell in the matrix; otherwise it would skew the range.
      val hasMissing = elemsSorted.size.toLong < n
      MinMaxImpl(
        if (hasMissing) math.min(minV, missingValue) else minV,
        if (hasMissing) math.max(maxV, missingValue) else maxV
      )
    }
  }
}
