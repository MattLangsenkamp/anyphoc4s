package org.dprl.anyphoc4s.splits

import org.dprl.anyphoc4s.geo.Geo.*
import org.dprl.anyphoc4s.model.{Geo2DMeta, Geo2DToken, Geo2DTokenSet, HorzSpec}
import org.locationtech.jts.geom.{Geometry, Polygon}

case class HorzSplit(levelNum: Int, splitNum: Int, spec: HorzSpec, tokenSet: Geo2DTokenSet) extends Geo2DSplit {
  val meta: Geo2DMeta = tokenSet.tokenSetMeta
  lazy val phocRegion: Polygon = {
    assert(spec.angle < 90 && spec.angle > -90)
    val tanAng = math.tan(math.toRadians(spec.angle)).asInstanceOf[Float]
    meta.boundingBox.centroid.y
    val totalSplits = levelNum + 1
    val linSpaceNum = totalSplits + 1
    val splits = linSpace(meta.boundingBox.minX, meta.boundingBox.maxX, linSpaceNum)
    val pos = if (spec.angle > 0) true else false
    var y1Bottom = shiftPointFromTan(splits(splitNum), tanAng, meta.boundingBox.centroid.x, !pos)
    var y2Bottom = shiftPointFromTan(splits(splitNum + 1), tanAng, meta.boundingBox.centroid.x, pos)
    var y1Top = shiftPointFromTan(splits(splitNum), tanAng, meta.boundingBox.centroid.x, !pos)
    var y2Top = shiftPointFromTan(splits(splitNum + 1), tanAng, meta.boundingBox.centroid.x, pos)

    val x1Bottom, x1Top = meta.boundingBox.minX
    val x2Bottom, x2Top = meta.boundingBox.maxX

    if (splitNum == 0 && spec.angle != 0) {
      y1Bottom = math.max(meta.boundingBox.minY, y1Bottom)
      y2Bottom = math.max(meta.boundingBox.minY, y2Bottom)
      polygonFromPoints(x1Bottom, y2Bottom, x2Bottom, y2Bottom, x1Top, y1Top, x2Top, y2Top)
        .intersection(meta.boundingBox.poly).asInstanceOf[Polygon]
    } else if (splitNum == levelNum && spec.angle != 0) {
      y1Top = math.min(meta.boundingBox.maxY, y1Top)
      y2Top = math.min(meta.boundingBox.maxY, y2Top)
      polygonFromPoints(x1Bottom, y2Bottom, x2Bottom, y2Bottom, x1Top, y1Top, x2Top, y2Top)
        .intersection(meta.boundingBox.poly).asInstanceOf[Polygon]
    } else if (spec.angle != 0) {
      polygonFromPoints(x1Bottom, y1Bottom, x2Bottom, y2Bottom, x1Top, y1Top, x2Top, y2Top)
        .intersection(meta.boundingBox.poly).asInstanceOf[Polygon]
    } else {
      polygonFromPoints(x1Bottom, y1Bottom, x2Bottom, y2Bottom, x1Top, y1Top, x2Top, y2Top)
    }
  }
}

