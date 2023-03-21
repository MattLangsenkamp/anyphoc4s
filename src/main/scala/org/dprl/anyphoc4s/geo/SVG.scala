package org.dprl.anyphoc4s.geo

import org.locationtech.jts.geom.{Coordinate, Geometry, Point, Polygon}

import scala.collection.mutable

extension(poly: Polygon) {
  def toSVG(
             scaleFactor: Float=1,
             fillColor: String="#66cc99",
             fillOpacity: Float=.3,
             strokeColor: String="#000000",
             strokeOpacity: Float=.7,
             strokeWidth: Float=20,
           ): String = {

    val stringBuilder: mutable.StringBuilder = new mutable.StringBuilder()
    // add string for exterior sequence
    val q = poly.getCoordinates
    val qq = poly.getExteriorRing.getCoordinateSequence.toCoordinateArray.toList
    stringBuilder.append(
      coordListToString(
        poly.getExteriorRing.getCoordinateSequence.toCoordinateArray.toList))


    // add strings for all interiors after
    for (curRingN <- 0 until poly.getNumInteriorRing) {
      val curRing = poly.getInteriorRingN(curRingN)
      val coords = curRing.getCoordinateSequence.toCoordinateArray.toList
      stringBuilder.append(coordListToString(coords))
    }
    f"<path fill-rule=\"evenodd\" fill=\"$fillColor\" stroke=\"$strokeColor\" "+
      f"stroke-width=\"${strokeWidth * scaleFactor}\" fill-opacity=\"$fillOpacity\" " +
      f"stroke-opacity=\"$strokeOpacity\" d=\"${stringBuilder.toString}\" />"
  }
}

def coordListToString(l: List[Coordinate]): String = {

  if (l.isEmpty) return ""
  val first = l.head
  val rest = l.tail


  val firstStr = f"${first.x},${first.y}"
  val restStr = rest.map(c => f"${c.x},${c.y}").mkString(" L ")
  f"M $firstStr L $restStr z"
}

extension(point: Point) {
  def toSVGPoint(
             scaleFactor: Float=1,
             fillColor: String="#66cc99",
             fillOpacity: Float=1,
             strokeColor: String="#000000",
             strokeOpacity: Float=.7,
             strokeWidth: Float=20,
           ): String =
    f"<circle cx=\"${point.getX}\" cy=\"${point.getY}\" r=\"${3*scaleFactor}\" stroke=\"$strokeColor\" " +
      f"stroke-width=\"$strokeWidth\" fill=\"$fillColor\" opacity=\"$fillOpacity\" stroke-opacity=\"$strokeOpacity\"/>"
}



