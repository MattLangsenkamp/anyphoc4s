package org.dprl.anyphoc4s
import org.dprl.anyphoc4s.geo.*
import org.dprl.anyphoc4s.model.{Geo2DSpec, Geo2DToken, Spec, Token}
import org.locationtech.jts.geom.{Point, Polygon}

trait VisualizeToken[A <: Token] {

  def visualize(token: A): String
}

object VisualizeToken{

  def apply[A <: Token](implicit ev: VisualizeToken[A]): VisualizeToken[A] = ev

  given jtsGeometry: VisualizeToken[Geo2DToken] with
    override def visualize(token: Geo2DToken): String = {
      token.repr(token.repr2d) match
        // fill:blue;stroke:red;stroke-width:5;fill-opacity:0.05;stroke-opacity:1
        case t: Polygon => t.toSVG(fillOpacity = 0.05, fillColor = "blue", strokeColor = "red")
        case t: Point => t.toSVGPoint()
    }
}
