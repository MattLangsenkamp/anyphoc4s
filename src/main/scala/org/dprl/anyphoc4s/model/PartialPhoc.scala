package org.dprl.anyphoc4s.model

import org.dprl.anyphoc4s.model.Phoc

case class PartialPhoc(map: Map[String, List[Byte]]) {

  extension (b: Boolean)
    def toInt: Int =
      if (b) 1 else 0

  def toPhoc(firstBit: Boolean = true): Phoc =

    Phoc(this.map.foldLeft(this.map){
      (accMap, curKey) => accMap + (curKey._1 -> (List.fill(firstBit.toInt)(1.toByte) ++ curKey._2))
    })

  def append(other: PartialPhoc): PartialPhoc =
    PartialPhoc(other.map.foldLeft(this.map) {
      (accMap, curKey) => accMap + (curKey._1 -> (accMap(curKey._1) ++ other.map(curKey._1)))
    })
}
