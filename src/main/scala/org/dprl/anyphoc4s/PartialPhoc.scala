package org.dprl.anyphoc4s

case class PartialPhoc(map: Map[String, List[Byte]]) {
  def toPhoc: Phoc =

    Phoc(Map("" -> List()))

  def append(other: PartialPhoc): PartialPhoc = PartialPhoc(Map("" -> List()))
}
