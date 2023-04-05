package org.dprl.anyphoc4s.model

import org.locationtech.jts.index.strtree.STRtree
import org.locationtech.jts.index.quadtree.Quadtree
import com.github.plokhotnyuk.rtree2d.core._
import EuclideanPlane._
import org.dprl.anyphoc4s.*

trait TokenSet {
  def tokenSetMeta: TokenSetMeta

  def tokens: List[Token]

}

class Geo2DTokenSet(val tokenSetMeta: Geo2DMeta, val tokens: List[Geo2DToken]) extends TokenSet {
  def toGeo2DTokenSetRTree2D: Geo2DTokenSetRTree2D = Geo2DTokenSetRTree2D(tokenSetMeta, tokens)

  def toGeo2DTokenSetSTRTreeJTS: Geo2DTokenSetSTRTreeJTS = Geo2DTokenSetSTRTreeJTS(tokenSetMeta, tokens)

  def toGeo2DTokenSetQuadTree: Geo2DTokenSetQuadTree = Geo2DTokenSetQuadTree(tokenSetMeta, tokens)

}

case class Geo2DTokenSetRTree2D(
                                 override val tokenSetMeta: Geo2DMeta,
                                 override val tokens: List[Geo2DToken]
                               ) extends Geo2DTokenSet(tokenSetMeta, tokens) {

  val rTree: RTree[Geo2DToken] = {
    val entries: Seq[RTreeEntry[Geo2DToken]] =
      tokens.map(t => entry(
        t.boundingBox.minX.toFloat,
        t.boundingBox.minY.toFloat,
        t.boundingBox.maxX.toFloat,
        t.boundingBox.maxY.toFloat,
        t)
      )
    RTree(entries)
  }
}

case class Geo2DTokenSetSTRTreeJTS(
                                 override val tokenSetMeta: Geo2DMeta,
                                 override val tokens: List[Geo2DToken]
                               ) extends Geo2DTokenSet(tokenSetMeta, tokens) {
  val rTree: STRtree = {
    val tree = new STRtree(tokens.length)
    tokens.foreach(t => tree.insert(t.boundingBox.poly.getEnvelopeInternal, t))
    tree
  }
}

case class Geo2DTokenSetQuadTree(
                                    override val tokenSetMeta: Geo2DMeta,
                                    override val tokens: List[Geo2DToken]
                                  ) extends Geo2DTokenSet(tokenSetMeta, tokens) {
  val rTree: Quadtree = {
    val tree = new Quadtree()
    tokens.foreach(t => tree.insert(t.boundingBox.poly.getEnvelopeInternal, t))
    tree
  }
}
