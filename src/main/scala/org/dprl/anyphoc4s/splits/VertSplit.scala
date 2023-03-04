package org.dprl.anyphoc4s.splits
import org.dprl.anyphoc4s.geo.Geo.*
import org.dprl.anyphoc4s.{Geo2DMeta, Geo2DToken, Geo2DTokenSet, VertSpec}
import org.locationtech.jts.geom.{Geometry, Polygon}

case class VertSplit(levelNum: Int, splitNum: Int, spec: VertSpec, tokenSet: Geo2DTokenSet) extends Geo2DSplit {
  val meta: Geo2DMeta = tokenSet.tokenSetMeta
  lazy val phocRegion: Polygon = {
    assert(spec.angle < 90 && spec.angle > -90)
    val tanAng = math.tan(math.toRadians(spec.angle)).asInstanceOf[Float]
    meta.boundingBox.centroid.y
    val totalSplits = levelNum + 1
    val linSpaceNum = totalSplits + 1
    val splits = linSpace(meta.boundingBox.minX, meta.boundingBox.maxX, linSpaceNum)
    val pos = if (spec.angle > 0) true else false
    var x1Bottom = shiftPointFromTan(splits(splitNum), tanAng, meta.boundingBox.centroid.y, pos)
    var x2Bottom = shiftPointFromTan(splits(splitNum + 1), tanAng, meta.boundingBox.centroid.y, pos)
    var x1Top = shiftPointFromTan(splits(splitNum), tanAng, meta.boundingBox.centroid.y, !pos)
    var x2Top = shiftPointFromTan(splits(splitNum + 1), tanAng, meta.boundingBox.centroid.y, !pos)

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
