package org.dprl.anyphoc4s.geo

import org.dprl.anyphoc4s.geo.Geo.polygonFromBoundingBox
import org.dprl.anyphoc4s.geo.{BoundingBox, Centroid}
import org.locationtech.jts.geom.*

object Geo {

  implicit val geoFact: GeometryFactory = new GeometryFactory(new PrecisionModel())

  def polygonFromBoundingBox(boundingBox: BoundingBox)(implicit gf: GeometryFactory): Polygon = {
    val p1 = new Coordinate(boundingBox.minX,boundingBox.minY)
    val p2 = new Coordinate(boundingBox.maxX,boundingBox.minY)
    val p3 = new Coordinate(boundingBox.maxX,boundingBox.maxY)
    val p4 = new Coordinate(boundingBox.minX,boundingBox.maxY)
    val coordinates = Array(p1, p2, p3, p4)
    gf.createPolygon(coordinates)
  }

  def lineFromBoundingBox(boundingBox: BoundingBox)(implicit gf: GeometryFactory): Polygon = {

    val y = boundingBox.centroid.x
    val yTop = y + boundingBox.height/1000
    val yBottom = y + boundingBox.height/1000

    val p1 = new Coordinate(boundingBox.minX,yBottom)
    val p2 = new Coordinate(boundingBox.maxX,yBottom)
    val p3 = new Coordinate(boundingBox.maxX,yTop)
    val p4 = new Coordinate(boundingBox.minX,yTop)
    val coordinates = Array(p1, p2, p3, p4)
    gf.createPolygon(coordinates)

  }

  def polygonFromPoints(bottomX1: Float, bottomY1: Float,
                        bottomX2: Float, bottomY2: Float,
                        topX1: Float, topY1: Float,
                        topX2: Float, topY2: Float)(implicit gf: GeometryFactory): Polygon = {
    val bottom1 = new Coordinate(bottomX1, bottomY1)
    val bottom2 = new Coordinate(bottomX2, bottomY2)
    val top1 = new Coordinate(topX1, topY1)
    val top2 = new Coordinate(topX2, topY2)
    val coordinates = Array(bottom1, bottom2, top1, top2)
    gf.createPolygon(coordinates)
  }

  def boundingBoxFromPoints(bottomX1: Float, bottomY1: Float,
                            bottomX2: Float, bottomY2: Float,
                            topX1: Float, topY1: Float,
                            topX2: Float, topY2: Float): BoundingBox = {
    val minX = Array(bottomX1, bottomX2, topX1, topX2).min
    val maxX = Array(bottomX1, bottomX2, topX1, topX2).max
    val minY = Array(bottomY1, bottomY2, topY1, topY2).min
    val maxY = Array(bottomY1, bottomY2, topY1, topY2).max
    BoundingBox(minX, minY, maxX, maxY)
  }

  def pointFromBoundingBox(boundingBox: BoundingBox): Point = boundingBox.poly.getCentroid

  // def entryFromBoundingBox(boundingBox: BoundingBox): (Float, Float, Float, Float) = ???
  // def entryFromBounds
  def linSpace(start: Float, stop: Float, num: Int): List[Float] = {
    if (num == 0) {
      List()
    } else if (num == 1) {
      List(start)
    } else {
      val dist = stop-start
      val stepSize = dist/(num-1)
      List.fill(num)(start).zipWithIndex.map{ case (_, i) => start + stepSize*i }
    }
  }

  def getCircleSteps(start: Float, stop: Float, steps: Int): List[Float] = {
    linSpace(0, stop-start, steps).reverse
  }

  def shiftPointFromTan(point: Float, tanAng: Float, adjacent: Float, pos: Boolean): Float =
    if (pos) {
      point + tanAng*adjacent
    } else {
      point - tanAng*adjacent
    }
}
