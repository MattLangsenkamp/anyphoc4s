package org.dprl.anyphoc4s.geo
import org.dprl.anyphoc4s.geo.Geo.{lineFromBoundingBox, polygonFromBoundingBox}
import org.locationtech.jts.geom.{Coordinate, CoordinateSequence, Point, Polygon}
import org.locationtech.jts.geom.util.AffineTransformation
import org.dprl.anyphoc4s.geo.Geo.geoFact


case class BoundingBox(minX: Double, minY: Double, maxX: Double, maxY:Double, scale: Double = 1) {
  def toTuple: (Double, Double, Double, Double) = (minX, minY, maxX, maxY)
  lazy val centroid: Centroid = Centroid(width/2+minX, height/2+minY)
  lazy val width: Double = maxX - minX
  lazy val height: Double = maxY - minY
  lazy val poly: Polygon =  {
    val boundingBox = polygonFromBoundingBox(this)(Geo.geoFact)
    if (scale != 1)
      val scaleTransformation = AffineTransformation.scaleInstance(scale, scale, centroid.x, centroid.y)
      scaleTransformation.transform(boundingBox).asInstanceOf[Polygon]
    else
      boundingBox
  }
  lazy val line: Polygon = {
    val lineBounds = lineFromBoundingBox(this)(Geo.geoFact)
    if (scale != 1)
      val scaleTransformation = AffineTransformation.scaleInstance(scale, 1, centroid.x, centroid.y)
      scaleTransformation.transform(lineBounds).asInstanceOf[Polygon]
    else
      lineBounds
  }
  lazy val centroidPoint: Point = Geo.geoFact.createPoint(new Coordinate(centroid.x, centroid.y))
}

case class Centroid(x: Double, y: Double)
