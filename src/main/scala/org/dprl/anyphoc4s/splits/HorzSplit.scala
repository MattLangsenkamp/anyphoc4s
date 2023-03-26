package org.dprl.anyphoc4s.splits

import org.dprl.anyphoc4s.geo.Geo.*
import org.dprl.anyphoc4s.model.{Geo2DMeta, Geo2DToken, Geo2DTokenSet, HorzSpec}
import org.locationtech.jts.geom.{Geometry, Polygon}

case class HorzSplit(levelNum: Int, splitNum: Int, spec: HorzSpec, tokenSet: Geo2DTokenSet) extends Geo2DSplit {
  val meta: Geo2DMeta = tokenSet.tokenSetMeta
  lazy val phocRegion: Polygon = {
    assert(spec.angle < 90 && spec.angle > -90)
    val tanAng = math.tan(spec.angle.abs.toRadians).toFloat
    val totalSplits = levelNum
    val linSpaceNum = totalSplits + 1
    val splits = linSpace(meta.boundingBox.minY, meta.boundingBox.maxY, linSpaceNum)
    val pos = if (spec.angle > 0) true else false
    var (y1Bottom, y2Bottom, y1Top, y2Top) = if (splits.length > 1)
      val y1Bottom_ = shiftPointFromTan(splits(splitNum), tanAng, meta.boundingBox.width / 2, pos)
      val y1Top_ = shiftPointFromTan(splits(splitNum+1), tanAng, meta.boundingBox.width / 2, pos)
      val y2Bottom_ = shiftPointFromTan(splits(splitNum), tanAng, meta.boundingBox.width / 2, !pos)
      val y2Top_ = shiftPointFromTan(splits(splitNum+1), tanAng, meta.boundingBox.width / 2, !pos)
      (y1Bottom_, y2Bottom_, y1Top_, y2Top_)
    else
      val y1Bottom_ = meta.boundingBox.minY
      val y2Bottom_ = meta.boundingBox.maxY
      val y1Top_ = meta.boundingBox.maxY
      val y2Top_ = meta.boundingBox.minY
      (y1Bottom_, y2Bottom_, y1Top_, y2Top_)

    val x1Bottom, x1Top = meta.boundingBox.minX
    val x2Bottom, x2Top = meta.boundingBox.maxX

    if (spec.angle < 0) {
      if (splitNum == 0)
        y1Bottom = meta.boundingBox.minY
        y2Bottom = meta.boundingBox.minY
      if (splitNum == levelNum - 1)
        y1Top = meta.boundingBox.maxY
        y2Top = meta.boundingBox.maxY
      polygonFromPoints(x1Bottom, y1Bottom, x2Bottom, y2Bottom, x1Top, y1Top, x2Top, y2Top)
        .intersection(meta.boundingBox.poly).asInstanceOf[Polygon]
    } else if (spec.angle > 0) {
      if (splitNum == levelNum - 1)
        y2Top = math.max(meta.boundingBox.maxY, y2Top)
      if (splitNum == 0)
        y1Bottom = math.min(meta.boundingBox.minY, y1Bottom)
      polygonFromPoints(x1Bottom, y1Bottom, x2Bottom, y2Bottom, x1Top, y1Top, x2Top, y2Top)
        .intersection(meta.boundingBox.poly).asInstanceOf[Polygon]
    } else {
      polygonFromPoints(x1Bottom, y1Bottom, x2Bottom, y2Bottom, x1Top, y1Top, x2Top, y2Top)
    }
  }
}

