package org.dprl.anyphoc4s

trait TokenSet {
  def tokenSetMeta: TokenSetMeta

  def tokens: List[Token]

  def filePath: Option[String]

  def page: Option[Int]
}

class Geo2DTokenSet(val tokenSetMeta: Geo2DMeta, val tokens: List[Geo2DToken], val filePath: Option[String], val page: Option[Int]) extends TokenSet {
  def toGeo2DTokenSetRTree2D: Geo2DTokenSetRTree2D = ???
}

case class Geo2DTokenSetRTree2D(
                                 override val tokenSetMeta: Geo2DMeta,
                                 override val tokens: List[Geo2DToken],
                                 override val filePath: Option[String],
                                 override val page: Option[Int]
                               ) extends Geo2DTokenSet(tokenSetMeta, tokens, filePath, page)

trait CompoundTokenSet

case class ProDoc(filename: String, tokenSets: List[Geo2DTokenSet]) extends CompoundTokenSet