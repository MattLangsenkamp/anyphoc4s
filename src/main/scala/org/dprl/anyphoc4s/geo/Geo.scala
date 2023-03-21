package org.dprl.anyphoc4s.geo

import org.dprl.anyphoc4s.geo.Geo.polygonFromBoundingBox
import org.dprl.anyphoc4s.geo.{BoundingBox, Centroid}
import org.locationtech.jts.geom.*

object Geo {

  implicit val geoFact: GeometryFactory = new GeometryFactory(new PrecisionModel())

  /**
   * Given a BoundingBox class create a Polygon Object that represents it.
   *
   * @param boundingBox the bounding box class
   * @param gf the implicitly passed in GeometryFactory
   * @return a polygon representing the bounding box
   */
  def polygonFromBoundingBox(boundingBox: BoundingBox)(using gf: GeometryFactory): Polygon = {
    val p1 = new Coordinate(boundingBox.minX,boundingBox.minY)
    val p2 = new Coordinate(boundingBox.maxX,boundingBox.minY)
    val p3 = new Coordinate(boundingBox.maxX,boundingBox.maxY)
    val p4 = new Coordinate(boundingBox.minX,boundingBox.maxY)
    val coordinates = Array(p1, p2, p3, p4, p1)
    gf.createPolygon(coordinates)
  }

  /**
   * Given a Bounding Box class calculate the line presentation,
   * which is the horizontal span of the bounding box, centered at the centroid of the bounding box
   *
   * @param boundingBox The bounding box class
   * @param gf the implicitly passed in GeometryFactory
   * @return a polygon representing the horizontal span centered at the centroid
   */
  def lineFromBoundingBox(boundingBox: BoundingBox)(using gf: GeometryFactory): Polygon = {

    val y = boundingBox.centroid.y
    val yTop = y + boundingBox.height/1000
    val yBottom = y - boundingBox.height/1000

    val p1 = new Coordinate(boundingBox.minX,yBottom)
    val p2 = new Coordinate(boundingBox.maxX,yBottom)
    val p3 = new Coordinate(boundingBox.maxX,yTop)
    val p4 = new Coordinate(boundingBox.minX,yTop)
    val coordinates = Array(p1, p2, p3, p4, p1)
    gf.createPolygon(coordinates)

  }

  /**
   *
   * @param bottomX1 the x value of the left-most bottom point
   * @param bottomY1 the y value of the left-most bottom point
   * @param bottomX2 the x value of the right-most bottom point
   * @param bottomY2 the y value of the right-most bottom point
   * @param topX1 the x value of the left-most top point
   * @param topY1 the y value of the left-most top point
   * @param topX2 the x value of the right-most top point
   * @param topY2 the y value of the right-most top point
   * @param gf the implicitly passed in GeometryFactory
   * @return A Polygon representing a bounding box
   */
  def polygonFromPoints(bottomX1: Double, bottomY1: Double,
                        bottomX2: Double, bottomY2: Double,
                        topX1: Double, topY1: Double,
                        topX2: Double, topY2: Double)(implicit gf: GeometryFactory): Polygon = {
    val bottom1 = new Coordinate(bottomX1, bottomY1)
    val bottom2 = new Coordinate(bottomX2, bottomY2)
    val top1 = new Coordinate(topX1, topY1)
    val top2 = new Coordinate(topX2, topY2)
    val coordinates = Array(bottom1, bottom2, top2, top1, bottom1)
    gf.createPolygon(coordinates)
  }

  /**
   *
   * @param bottomX1 the x value of the left-most bottom point
   * @param bottomY1 the y value of the left-most bottom point
   * @param bottomX2 the x value of the right-most bottom point
   * @param bottomY2 the y value of the right-most bottom point
   * @param topX1    the x value of the left-most top point
   * @param topY1    the y value of the left-most top point
   * @param topX2    the x value of the right-most top point
   * @param topY2    the y value of the right-most top point
   * @return A BoundingBox representing a bounding box
   */
  def boundingBoxFromPoints(bottomX1: Double, bottomY1: Double,
                            bottomX2: Double, bottomY2: Double,
                            topX1: Double, topY1: Double,
                            topX2: Double, topY2: Double): BoundingBox = {
    val minX = Array(bottomX1, bottomX2, topX1, topX2).min
    val maxX = Array(bottomX1, bottomX2, topX1, topX2).max
    val minY = Array(bottomY1, bottomY2, topY1, topY2).min
    val maxY = Array(bottomY1, bottomY2, topY1, topY2).max
    BoundingBox(minX, minY, maxX, maxY)
  }

  /** the linspace function as defined in NumPy, but without all of the functionality present in NumPy.
   *
   * returns a list of equally spaced points along an interval created by the interval [start, stop].
   * The start and stop parameters will always be in the list as the interval is inclusive of stop and start
   *
   * @param start first entry in the returned list, if num is one then the list will be List(start)
   * @param stop last entry in the returned list, if num is 2 then the list will be List(start, stop)
   * @param num the number of equally spaced points along the interval
   * @return a list of doubles
   *
   * {{{
   *   val l1 = linSpace(1,5,1) // List(1.0)
   *   val l2 = linSpace(1,5,2) // List(1.0, 5.0)
   *   val l3 = linSpace(1,10,4) // List(1.0, 4.0, 7.0, 10.0)
   * }}}
   */
  def linSpace(start: Double, stop: Double, num: Int): List[Double] = {
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

  /** the magnitude of the length from the 0 to
   * a set of equally spaced points along the interval [0, max-min]
   * ordered from largest to smallest
   *
   * @param min the minimum value along the current axis
   * @param max the maximum value along the current axis
   * @param steps The number of intervals
   *
   * @return a List of doubles representing the magnitudes
   * {{{
   *    val s1 = getCircleSteps(-10, -5, 4) // List(5.0, 3.3333333333333335, 1.6666666666666667, 0.0)
   *    val s2 = getCircleSteps(-10, -5, 1) // List(0.0)
   * }}}
   */
  def getCircleSteps(min: Double, max: Double, steps: Int): List[Double] = {
    linSpace(0, max-min, steps).reverse
  }

  /**
   * Uses the SOH-CAH-TOA formula to adjust a point based on the
   * length of the adjacent edge of the triangle and the tangent of the angle (not radians)
   *
   * @param point one dimension of the point
   * @param tanAng the tangent angle
   * @param adjacent the length of the adjacent side
   * @param pos a flag dictating which way to adjust the dimension of the point
   * @return
   */
  def shiftPointFromTan(point: Double, tanAng: Double, adjacent: Double, pos: Boolean): Double =
    if (pos) {
      point + tanAng * adjacent
    } else {
      point - tanAng * adjacent
    }
}
