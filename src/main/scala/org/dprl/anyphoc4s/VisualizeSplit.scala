package org.dprl.anyphoc4s
import org.dprl.anyphoc4s.geo.*
import org.dprl.anyphoc4s.splits.{Geo2DSplit, Split}

trait VisualizeSplit[A <: Split] {

  def visualize(split: A): String
}

object VisualizeSplit {

  def apply[A <: Split](implicit ev: VisualizeSplit[A]): VisualizeSplit[A] = ev

  given jtsGeometry: VisualizeSplit[Geo2DSplit] with
    override def visualize(split: Geo2DSplit): String =
      split.phocRegion.toSVG()
}
