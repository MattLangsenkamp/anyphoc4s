package org.dprl.anyphoc4s.geo
import org.dprl.anyphoc4s.geo.Geo.{lineFromBoundingBox, polygonFromBoundingBox}
import org.locationtech.jts.geom.{Polygon, Point}


case class BoundingBox(minX: Float, minY: Float, maxX: Float, maxY:Float) {
  def toTuple: (Float, Float, Float, Float) = (minX, minY, maxX, maxY)
  lazy val centroid: Centroid = Centroid(width/2, height/2)
  lazy val width: Float = maxX-minX
  lazy val height: Float = maxY-minY
  lazy val poly: Polygon = polygonFromBoundingBox(this)(Geo.geoFact)
  lazy val line: Polygon = lineFromBoundingBox(this)(Geo.geoFact)
  lazy val centroidPoint: Point = ???
}

case class Centroid(x: Float, y: Float)
