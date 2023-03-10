package org.dprl.anyphoc4s.splits

import org.dprl.anyphoc4s.geo.Geo.*
import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DMeta, Geo2DToken, Geo2DTokenSet}
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, Polygon, PrecisionModel}
import org.locationtech.jts.geom.util.AffineTransformation

case class EllipseSplit(levelNum: Int, splitNum: Int, spec: EllipseSpec, tokenSet: Geo2DTokenSet) extends Geo2DSplit {
  val meta: Geo2DMeta = tokenSet.tokenSetMeta
  lazy val phocRegion: Polygon = {
    val nSpec = verifyConstraints(spec, meta)

    if (splitNum == 0) {
      val firstCircle: Polygon = getCircle(levelNum, splitNum, nSpec, meta)
      meta.boundingBox.poly.difference(firstCircle).asInstanceOf[Polygon]
    } else if (levelNum > splitNum) {
      val innerCircle = getCircle(levelNum, splitNum + 1, nSpec, meta)
      val outerCircle = getCircle(levelNum, splitNum, nSpec, meta)
      outerCircle.difference(innerCircle).asInstanceOf[Polygon]
    } else {
      getCircle(levelNum, splitNum, nSpec, meta)
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

    val gf = new GeometryFactory(new PrecisionModel())
    val p = gf.createPoint(new Coordinate(meta.boundingBox.centroid.x, meta.boundingBox.centroid.y))
    val rotateTrans = AffineTransformation.rotationInstance(spec.rotateDegree)
    val (fSplits, scaleTrans) = spec match {
      // Glue to both
      case EllipseSpec(_, _, _, _, true, true, false, _, _, _) =>
        if (meta.boundingBox.height > meta.boundingBox.width) {
          val splits = getCircleSteps(meta.boundingBox.centroid.x, meta.boundingBox.maxX, levelNum+1)
          val nScaleFactor = meta.boundingBox.width / meta.boundingBox.height
          val scaleTrans = AffineTransformation.scaleInstance(nScaleFactor, 1)
          (splits, scaleTrans)
        } else {
          val splits = getCircleSteps(meta.boundingBox.centroid.y, meta.boundingBox.maxY, levelNum+1)
          val nScaleFactor = meta.boundingBox.height / meta.boundingBox.width
          val scaleTrans = AffineTransformation.scaleInstance(1, nScaleFactor)
          (splits, scaleTrans)
        }
      // Glue to height
      case EllipseSpec(_, _, _, _, true, _, _, scale, _, _) =>
        val splits = getCircleSteps(meta.boundingBox.centroid.y, meta.boundingBox.maxY, levelNum+1)
        val scaleTrans = AffineTransformation.scaleInstance(scale, 1)
        (splits, scaleTrans)

      // Glue to width
      case EllipseSpec(_, _, _, _, _, true, _, scale, _, _) =>
        val splits = getCircleSteps(meta.boundingBox.centroid.x, meta.boundingBox.maxX, levelNum+1)
        val scaleTrans = AffineTransformation.scaleInstance(1, scale)
        (splits, scaleTrans)

      // glue to larger
      case EllipseSpec(_, _, _, _, false, false, _, scale, _, _) =>
        if (meta.boundingBox.height > meta.boundingBox.width) {
          val splits = getCircleSteps(meta.boundingBox.centroid.x, meta.boundingBox.maxX, levelNum+1)
          val scaleTrans = AffineTransformation.scaleInstance(scale, 1)
          (splits, scaleTrans)
        } else {
          val splits = getCircleSteps(meta.boundingBox.centroid.y, meta.boundingBox.maxY, levelNum+1)
          val scaleTrans = AffineTransformation.scaleInstance(1, scale)
          (splits, scaleTrans)
        }
    }

    rotateTrans.compose(scaleTrans).compose(rotateTrans).transform(
      p.buffer(fSplits(splitNum)).asInstanceOf[Polygon]
    ).asInstanceOf[Polygon]
  }
}
