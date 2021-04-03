package org.nspl
import org.nspl.data._

/* Provided various implicit conversions to PlotData */
trait ImplicitConversions {
  type PlotData = (DataSource, List[DataRenderer], LegendConfig)
  type PlotData3D = (DataSource, List[DataRenderer3D], LegendConfig)
  implicit def dsToTuple1[T, F: FC](
      ds: T
  )(implicit f: T => DataSource): PlotData =
    (ds, List(point()), NotInLegend)
  // implicit def ds3DToTuple1[T, F: FC](
  //     ds: T
  // )(implicit f: T => DataSource): PlotData3D =
  //   (ds, List(point3D()), NotInLegend)

  implicit def dsToTuple2a[T, F: FC](ds: (T, LegendConfig))(implicit
      f: T => DataSource
  ): PlotData =
    (ds._1, List(point()), ds._2)

  implicit def dsToTuple2b[T, F: FC](ds: (T, List[DataRenderer]))(implicit
      f: T => DataSource
  ): PlotData =
    (ds._1, ds._2, NotInLegend)

  implicit def dsToTuple3c[T, F: FC](ds: ((T, DataRenderer), LegendConfig))(
      implicit f: T => DataSource
  ): PlotData = (ds._1._1, List(ds._1._2), ds._2)

  implicit def dsToTuple3d[T, F: FC](
      ds: ((T, List[DataRenderer]), LegendConfig)
  )(implicit
      f: T => DataSource
  ): PlotData = (ds._1._1, ds._1._2, ds._2)

  implicit def dsToTuple2c[T, F: FC](ds: (T, DataRenderer))(implicit
      f: T => DataSource
  ): PlotData = (ds._1, List(ds._2), NotInLegend)

  implicit def dsToTuple3[T, F: FC](ds: (T, DataRenderer, LegendConfig))(
      implicit f: T => DataSource
  ): PlotData = (ds._1, List(ds._2), ds._3)

  implicit def dsToTuple3b[T, F: FC](ds: (T, List[DataRenderer], LegendConfig))(
      implicit f: T => DataSource
  ): PlotData = (ds._1, ds._2, ds._3)

  implicit def listConv1[T, F: FC](ds: Seq[(T, List[DataRenderer])])(implicit
      f: T => DataSource
  ): Seq[PlotData] =
    ds.map(x => (f(x._1), x._2, NotInLegend))

  implicit def listConv2[T, F: FC](
      ds: Seq[(T, List[DataRenderer], LegendConfig)]
  )(implicit
      f: T => DataSource
  ): Seq[PlotData] =
    ds.map(x => (f(x._1), x._2, x._3))

  implicit def listConv3[T, F: FC](ds: Seq[(T, DataRenderer, LegendConfig)])(
      implicit f: T => DataSource
  ): Seq[PlotData] =
    ds.map(x => (f(x._1), List(x._2), x._3))

  implicit def listConv2b[T, F: FC](ds: Seq[(T, DataRenderer)])(implicit
      f: T => DataSource
  ): Seq[PlotData] =
    ds.map(x => (f(x._1), List(x._2), NotInLegend))
}
