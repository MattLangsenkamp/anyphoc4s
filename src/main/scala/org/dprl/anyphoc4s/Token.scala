package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.Geo2DRepr.*
import org.dprl.anyphoc4s.geo.BoundingBox
import org.locationtech.jts.geom.Geometry


trait Token {
  def key: String
}

enum Geo2DRepr {
  case LINE, BBOX, CENTROID
}

case class Geo2DToken(key: String, boundingBox: BoundingBox) extends Token {
  def repr(r: Geo2DRepr): Geometry = r match {
    case LINE => boundingBox.line
    case BBOX => boundingBox.poly
    case CENTROID => boundingBox.centroidPoint
  }
}