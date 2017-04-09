package org.nspl

package object data {

  def linspace(min: Double, max: Double, n: Int) = 0 until n map { i =>
    min + (max - min) / n * i
  }

  def mean(s: Seq[Double]) = s.sum / s.size

  def sampleVariance(s: Seq[Double]) = {
    val m = mean(s)
    val n = s.size
    (s.foldLeft(0.0)((s, i) => (i - m) * (i - m) + s)) / (n - 1)
  }

  /**
    * Copyright (c) 2013 Saddle Development Team
    *
    * Licensed under the Apache License, Version 2.0 (the "License");
    * you may not use this file except in compliance with the License.
    * You may obtain a copy of the License at
    *
    *     http://www.apache.org/licenses/LICENSE-2.0
    *
    * Unless required by applicable law or agreed to in writing, software
    * distributed under the License is distributed on an "AS IS" BASIS,
    * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    * See the License for the specific language governing permissions and
    * limitations under the License.
    */
  // Adapted from Saddle
  def percentile(v: Seq[Double], percentiles: Seq[Double]): Seq[Double] = {
    val vf = v.filterNot(_.isNaN)
    assert(percentiles.forall(x => x >= 0.0 && x <= 1d), percentiles)
    if (vf.length == 0)
      Seq.fill(percentiles.size)(Double.NaN)
    else {
      val c = vf.length
      if (c == 1) Seq.fill(percentiles.size)(vf(0))
      else {
        val ns = percentiles.map { p =>
          p * (c + 1.0)
        }

        val s = vf.sorted
        val ks = ns.map(n => math.floor(n).toInt)
        val ds = ns zip ks map (x => x._1 - x._2)

        ks zip ds map {
          case (k, d) =>
            if (k <= 0) s(0)
            else if (k >= c) s.last
            else s(k - 1) + d * (s(k) - s(k - 1))
        }
      }
    }
  }
}
