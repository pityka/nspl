package org.nspl.data;

case class DataMatrix(rows: Array[Double], numCols: Int, numRows: Int)
    extends DataSource {

  val dimension = 3
  private val n = rows.size

  def iterator = new Iterator[Row] {
    var k = 0;
    def hasNext = k < n;

    def next = {
      if (!hasNext) throw new RuntimeException("!hasNext");

      val row = new Row {
        val K = k;
        val i = K / numCols;
        val j = K % numCols;

        def apply(l: Int) =
          if (l == 0) j.toDouble
          else if (l == 1) i.toDouble
          else rows(K)

        def allColumns = Vector(j, i, rows(K))

        def dimension = 3

        def label = ""

        override def toString = rows(K).toString
      }

      k += 1;
      row
    }
  }

  def columnMinMax(i: Int) = {
    if (i == 0)
      new MinMax {
        def min = 0.0
        def max = numCols - 1d
      } else if (i == 1) new MinMax {
      def min = 0.0
      def max = numRows - 1d
    } else {

      // var min = Double.MAX_VALUE;
      // var max = Double.MIN_VALUE;
      // int j = 0;
      // while (j < rows.length) {
      // 	double v = rows[j];
      // 	if (v < min) {
      // 		min = v;
      // 	}
      // 	if (v > max) {
      // 		max = v;
      // 	}
      // 	j+=1;
      // }

      // final double min1 = min;
      // final double max1 = max;

      new MinMax {
        def min = rows.min
        def max = rows.max
      }
    }
  }

}
