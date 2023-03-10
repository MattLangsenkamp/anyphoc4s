package org.dprl.anyphoc4s.model

import org.dprl.anyphoc4s.*

trait TokenSet {
  def tokenSetMeta: TokenSetMeta

  def tokens: List[Token]

}

class Geo2DTokenSet(val tokenSetMeta: Geo2DMeta, val tokens: List[Geo2DToken]) extends TokenSet {
  def toGeo2DTokenSetRTree2D: Geo2DTokenSetRTree2D = ???

}

case class Geo2DTokenSetRTree2D(
                                 override val tokenSetMeta: Geo2DMeta,
                                 override val tokens: List[Geo2DToken]
                               ) extends Geo2DTokenSet(tokenSetMeta, tokens)
