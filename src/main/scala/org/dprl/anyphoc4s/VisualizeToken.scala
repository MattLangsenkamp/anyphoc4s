package org.dprl.anyphoc4s
import org.dprl.anyphoc4s.geo.*
import org.dprl.anyphoc4s.model.{Geo2DSpec, Geo2DToken, Spec, Token}
import org.locationtech.jts.geom.{Point, Polygon}

trait VisualizeToken[A <: Token, B <: Spec] {

  def visualize(token: A, spec: B): String
}

object VisualizeToken{

  def apply[A <: Token, B <: Spec](implicit ev: VisualizeToken[A, B]): VisualizeToken[A, B] = ev

  given jtsGeometry: VisualizeToken[Geo2DToken, Geo2DSpec] with
    override def visualize(token: Geo2DToken, spec: Geo2DSpec): String = {
      token.repr(spec.repr) match
        case t: Polygon => t.toSVG()
        case t: Point => t.toSVGPoint()
    }
}
