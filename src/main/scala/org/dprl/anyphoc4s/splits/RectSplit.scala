package org.dprl.anyphoc4s.splits


import org.dprl.anyphoc4s.geo.Geo.getCircleSteps
import org.dprl.anyphoc4s.model.{Geo2DMeta, Geo2DTokenSet, RectSpec}
import org.dprl.anyphoc4s.splits.Geo2DSplit
import org.locationtech.jts.geom.{Coordinate, Polygon}
import org.locationtech.jts.util.GeometricShapeFactory

case class RectSplit(levelNum: Int, splitNum: Int, spec: RectSpec, tokenSet: Geo2DTokenSet) extends Geo2DSplit {
  val meta: Geo2DMeta = tokenSet.tokenSetMeta
  lazy val phocRegion: Polygon = {
    val nSpec = verifyConstraints(spec, meta)

    if (splitNum == 0) {
      val firstRectangle: Polygon = getRectangle(levelNum, splitNum, nSpec, meta)
      meta.boundingBox.poly.difference(firstRectangle).asInstanceOf[Polygon]
    } else if (levelNum > splitNum + 1) {
      val innerCircle = getRectangle(levelNum, splitNum, nSpec, meta)
      val outerCircle = getRectangle(levelNum, splitNum - 1, nSpec, meta)
      outerCircle.difference(innerCircle).asInstanceOf[Polygon]
    } else {
      getRectangle(levelNum, splitNum - 1, nSpec, meta)
    }
  }

  // returns Some(True) if width is larger Some(False) if height is larger
  private def verifyConstraints(spec: RectSpec, meta: Geo2DMeta): RectSpec = {
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

  private def getRectangle(levelNum: Int, splitNum: Int, spec: RectSpec, meta: Geo2DMeta): Polygon = {

    val fact = new GeometricShapeFactory()

    fact.setRotation(spec.rotateDegree.toRadians)
    fact.setCentre(new Coordinate(meta.boundingBox.centroid.x, meta.boundingBox.centroid.y))
    val (xDist, yDist) = spec match {
      // Glue to both
      case RectSpec(_, _, _, _, true, true, false, _, _, _) =>

        val xSplits = getCircleSteps(meta.boundingBox.minX, meta.boundingBox.maxX, levelNum + 1)
        val ySplits = getCircleSteps(meta.boundingBox.minY, meta.boundingBox.maxY, levelNum + 1)
        val xDist_ = xSplits(splitNum+1)
        val yDist_ = ySplits(splitNum+1)
        (xDist_, yDist_)
      // Glue to height
      case RectSpec(_, _, _, _, true, _, _, scale, _, _) =>
        val splits = getCircleSteps(meta.boundingBox.centroid.y, meta.boundingBox.maxY, levelNum)
        val yDist_ = splits(splitNum+1)
        val xDist_ = yDist_ * scale
        (xDist_, yDist_)
      // Glue to width
      case RectSpec(_, _, _, _, _, true, _, scale, _, _) =>
        val splits = getCircleSteps(meta.boundingBox.centroid.x, meta.boundingBox.maxX, levelNum)
        val xDist_ = splits(splitNum+1)
        val yDist_ = xDist_ * scale
        (xDist_, yDist_)
      // glue to larger
      case RectSpec(_, _, _, _, false, false, _, scale, _, _) =>
        if (meta.boundingBox.height > meta.boundingBox.width) {
          val splits = getCircleSteps(meta.boundingBox.centroid.y, meta.boundingBox.maxY, levelNum)
          val yDist_ = splits(splitNum+1)
          val xDist_ = yDist_ * scale
          (xDist_, yDist_)
        } else {
          val splits = getCircleSteps(meta.boundingBox.centroid.x, meta.boundingBox.maxX, levelNum)
          val xDist_ = splits(splitNum+1)
          val yDist_ = xDist_ * scale
          (xDist_, yDist_)
        }
    }
    // -1 is hack to make sure no multi-polygons are formed
    fact.setWidth(xDist - 1)
    fact.setHeight(yDist - 1)
    fact.createRectangle()
  }
}
