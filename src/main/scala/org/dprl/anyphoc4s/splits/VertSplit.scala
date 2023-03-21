package org.dprl.anyphoc4s.splits
import org.dprl.anyphoc4s.geo.Geo.*
import org.dprl.anyphoc4s.model.{Geo2DMeta, Geo2DToken, Geo2DTokenSet, VertSpec}
import org.locationtech.jts.geom.{Geometry, Polygon}

case class VertSplit(levelNum: Int, splitNum: Int, spec: VertSpec, tokenSet: Geo2DTokenSet) extends Geo2DSplit {
  val meta: Geo2DMeta = tokenSet.tokenSetMeta
  lazy val phocRegion: Polygon = {
    assert(spec.angle < 90 && spec.angle > -90)
    val tanAng = math.tan(math.toRadians(spec.angle)).toFloat
    val totalSplits = levelNum
    val linSpaceNum = totalSplits + 1
    val splits = linSpace(meta.boundingBox.minX, meta.boundingBox.maxX, linSpaceNum)
    val pos = if (spec.angle > 0) true else false
    var (x1Bottom, x2Bottom, x1Top, x2Top) = if (splits.length > 1)
      val x1Bottom_ = shiftPointFromTan(splits(splitNum), tanAng, meta.boundingBox.centroid.y, pos)
      val x2Bottom_ = shiftPointFromTan(splits(splitNum+1), tanAng, meta.boundingBox.centroid.y, pos)
      val x1Top_ = shiftPointFromTan(splits(splitNum), tanAng, meta.boundingBox.centroid.y, !pos)
      val x2Top_ = shiftPointFromTan(splits(splitNum+1), tanAng, meta.boundingBox.centroid.y, !pos)
      (x1Bottom_, x2Bottom_, x1Top_, x2Top_)
    else
      val x1Bottom_ = meta.boundingBox.minX
      val x2Bottom_ = meta.boundingBox.maxX
      val x1Top_ = meta.boundingBox.minX
      val x2Top_ = meta.boundingBox.maxX
      (x1Bottom_, x2Bottom_, x1Top_, x2Top_)

    val y1Bottom, y2Bottom = meta.boundingBox.minY
    val y1Top, y2Top = meta.boundingBox.maxY

    if (splitNum == 0 && spec.angle !=0) {
      x1Bottom = math.max(meta.boundingBox.minX, x1Bottom)
      x1Top = math.max(meta.boundingBox.minX, x1Top)
      polygonFromPoints(x1Bottom, y1Bottom, x2Bottom, y2Bottom, x1Top, y1Top, x2Top, y2Top)
        .intersection(meta.boundingBox.poly).asInstanceOf[Polygon]
    } else if (splitNum == levelNum && spec.angle !=0) {
      x2Bottom = math.min(meta.boundingBox.maxX, x2Bottom)
      x2Top = math.min(meta.boundingBox.maxX, x2Top)
      polygonFromPoints(x1Bottom, y1Bottom, x2Bottom, y2Bottom, x1Top, y1Top, x2Top, y2Top)
        .intersection(meta.boundingBox.poly).asInstanceOf[Polygon]
    } else if (spec.angle !=0) {
      polygonFromPoints(x1Bottom, y1Bottom, x2Bottom, y2Bottom, x1Top, y1Top, x2Top, y2Top)
        .intersection(meta.boundingBox.poly).asInstanceOf[Polygon]
    } else {
      polygonFromPoints(x1Bottom, y1Bottom, x2Bottom, y2Bottom, x1Top, y1Top, x2Top, y2Top)
    }
  }
}
