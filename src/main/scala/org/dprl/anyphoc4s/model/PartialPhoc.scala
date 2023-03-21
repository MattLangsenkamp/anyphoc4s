package org.dprl.anyphoc4s.model

import org.dprl.anyphoc4s.model.Phoc

case class PartialPhoc(map: Map[String, List[Byte]]) {
  def toPhoc: Phoc =

    Phoc(this.map.foldLeft(this.map){
      (accMap, curKey) => accMap + (curKey._1 -> (List(1.toByte) ++ curKey._2))
    })

  def append(other: PartialPhoc): PartialPhoc =
    PartialPhoc(other.map.foldLeft(this.map) {
      (accMap, curKey) => accMap + (curKey._1 -> (accMap(curKey._1) ++ other.map(curKey._1)))
    })
}
