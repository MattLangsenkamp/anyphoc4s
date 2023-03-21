package org.dprl.anyphoc4s.splits

import org.dprl.anyphoc4s.geo.Geo.*
import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DMeta, Geo2DToken, Geo2DTokenSet}
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, MultiPolygon, Polygon, PrecisionModel}
import org.locationtech.jts.geom.util.AffineTransformation
import org.locationtech.jts.util.GeometricShapeFactory
import math.abs

case class EllipseSplit(levelNum: Int, splitNum: Int, spec: EllipseSpec, tokenSet: Geo2DTokenSet) extends Geo2DSplit {
  val meta: Geo2DMeta = tokenSet.tokenSetMeta
  lazy val phocRegion: Polygon = {
    val nSpec = verifyConstraints(spec, meta)

    if (splitNum == 0) {
      val firstCircle: Polygon = getCircle(levelNum, splitNum, nSpec, meta)
      meta.boundingBox.poly.difference(firstCircle).asInstanceOf[Polygon]
    } else if (levelNum > splitNum + 1) {
      val innerCircle = getCircle(levelNum, splitNum, nSpec, meta)
      val outerCircle = getCircle(levelNum, splitNum-1, nSpec, meta)
      outerCircle.difference(innerCircle).asInstanceOf[Polygon]
    } else {
      getCircle(levelNum, splitNum-1, nSpec, meta)
    }
  }

  // returns Some(True) if width is larger Some(False) if height is larger
  private def verifyConstraints(spec: EllipseSpec, meta: Geo2DMeta): EllipseSpec = {
    assert(spec.glueToWidth || spec.glueToHeight || spec.glueToLarger)
    if (spec.scaleFactor > 0) assert(!spec.glueToWidth || !spec.glueToHeight)
    val glueTo = if (spec.glueToLarger) {
      assert(!spec.glueToHeight)
      assert(!spec.glueToWidth)
      if (meta.boundingBox.height > meta.boundingBox.width) {
        spec.copy(glueToHeight = true, glueToLarger = true, glueToWidth = false)
      } else {
        spec.copy(glueToHeight = false, glueToLarger = true, glueToWidth = true)
      }
    } else {
      spec
    }
    glueTo
  }

  private def getCircle(levelNum: Int, splitNum: Int, spec: EllipseSpec, meta: Geo2DMeta): Polygon = {

    val fact = new GeometricShapeFactory()

    fact.setRotation(spec.rotateDegree.toRadians)
    fact.setCentre(new Coordinate(meta.boundingBox.centroid.x, meta.boundingBox.centroid.y))
    val (xDist, yDist) = spec match {
      // Glue to both
      case EllipseSpec(_, _, _, _, true, true, false, _, _, _) =>

        val xSplits = getCircleSteps(meta.boundingBox.minX, meta.boundingBox.centroid.x, levelNum)
        val ySplits = getCircleSteps(meta.boundingBox.minY, meta.boundingBox.centroid.y, levelNum)
        val xDist_ = xSplits(splitNum)*2
        val yDist_ = ySplits(splitNum)*2
        (xDist_, yDist_)
      // Glue to height
      case EllipseSpec(_, _, _, _, true, _, _, scale, _, _) =>
        val splits = getCircleSteps(meta.boundingBox.minY, meta.boundingBox.centroid.y, levelNum)
        val yDist_ = splits(splitNum)*2
        val xDist_ = yDist_ * scale
        (xDist_, yDist_)
      // Glue to width
      case EllipseSpec(_, _, _, _, _, true, _, scale, _, _) =>
        val splits = getCircleSteps(meta.boundingBox.minX, meta.boundingBox.centroid.x, levelNum)
        val xDist_ = splits(splitNum)*2
        val yDist_ = xDist_ * scale
        (xDist_, yDist_)
      // glue to larger
      case EllipseSpec(_, _, _, _, false, false, _, scale, _, _) =>
        if (meta.boundingBox.height > meta.boundingBox.width) {
          val splits = getCircleSteps(meta.boundingBox.minY,meta.boundingBox.centroid.y, levelNum)
          val yDist_ = splits(splitNum)*2
          val xDist_ = yDist_ *scale
          (xDist_, yDist_)
        } else {
          val splits = getCircleSteps(meta.boundingBox.minX, meta.boundingBox.centroid.x, levelNum)
          val xDist_ = splits(splitNum)*2
          val yDist_ = xDist_ * scale
          (xDist_, yDist_)
        }
    }
    // -1 is hack to make sure no multi-polygons are formed
    fact.setWidth(xDist-1)
    fact.setHeight(yDist-1)
    fact.createEllipse()
  }
}
