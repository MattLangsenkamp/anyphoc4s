package org.dprl.anyphoc4s
import org.dprl.anyphoc4s.geo.*
import org.dprl.anyphoc4s.splits.{Geo2DSplit, Split}
import org.jsoup.Jsoup


trait VisualizeSplit[A <: Split] {

  def visualize(split: A, splitNum: Int): String
}

object VisualizeSplit {

  private val levelColorList: List[String] = List(
    "#FF0000", "#FF6400", "#FFD900", "#D1FF00", "#87FF00",
    "#00FF0D", "#00FFAB", "#00D8FF", "#0057FF", "#7800FF", "#D600FF"
  )

  def apply[A <: Split](implicit ev: VisualizeSplit[A]): VisualizeSplit[A] = ev

  given jtsGeometry: VisualizeSplit[Geo2DSplit] with
    override def visualize(split: Geo2DSplit, splitNum: Int): String =
      val color = levelColorList(splitNum % levelColorList.length)
      split.phocRegion.toSVG(fillColor = color)
}
