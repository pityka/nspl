package org.nspl.data;

case class DataTable(rows: Array[Double], numCols: Int)
    extends DataSourceWithQuantiles {

  def numRows = rows.length / numCols

  def dimension = numCols

  def iterator =
    if (rows.isEmpty || numCols == 0) Iterator.empty
    else
      new Iterator[Row] {
        var j = 0;
        val n = rows.length / numCols;
        def hasNext = j < n
        def next() = {
          val row = new Row {
            val off = j;
            def apply(i: Int) = rows(off * numCols + i)
            def allColumns =
              rows.toIndexedSeq
                .slice(off * numCols, off * numCols + numCols)
            def dimension = numCols
            def label = allColumns.mkString("(", ",", ")")
            override def toString = label
          }
          j += 1
          row;
        }
      }

  private def computeMinMax(i: Int) = {
    var j = i;
    var min = Double.MaxValue;
    var max = Double.MinValue;
    while (j < rows.length) {
      val v = rows(j);
      if (v < min) {
        min = v;
      }
      if (v > max) {
        max = v;
      }
      j += numCols;
    }

    val min1 = min;
    val max1 = max;

    MinMaxImpl(min = min1, max = max1)
  }

  private val minMaxes = 0 until numCols map (computeMinMax) toVector

  def columnMinMax(i: Int) = Some(minMaxes(i))

  def quantilesOfColumn(i: Int, qs: Vector[Double]) = {
    val v = iterator.map(_(i)).toSeq
    percentile(v, qs).toVector
  }

}
