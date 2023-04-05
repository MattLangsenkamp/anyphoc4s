package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DToken, Geo2DTokenSet, Geo2DTokenSetRTree2D, PartialPhoc, Token, TokenSet}
import org.dprl.anyphoc4s.splits.{EllipseSplit, Geo2DSplit, HorzSplit, RectSplit, Split, VertSplit}

import Math.max

trait ExtractPhoc[A <: TokenSet, B <: Split] {
  def extract(tokenSet: A, splits: List[List[B]]): PartialPhoc
}

object ExtractPhoc {

  def apply[A <: TokenSet, B <: Split](implicit ev: ExtractPhoc[A, B]): ExtractPhoc[A, B] = ev

  given RTree2DExtract: ExtractPhoc[Geo2DTokenSetRTree2D, Geo2DSplit] with
    override def extract(tokenSet: Geo2DTokenSetRTree2D, splits: List[List[Geo2DSplit]]): PartialPhoc =
      val m = scala.collection.mutable.Map[String, List[Byte]]()
      // membership testing function
      val f = TestMembership[Geo2DSplit, Geo2DToken].test
      val fill = List.fill[Byte]

      for (level <- splits) {
        for (split <- level) {
          val env = split.phocRegion.getEnvelopeInternal
          val (minX, minY, maxX, maxY) = (env.getMinX.toFloat, env.getMinY.toFloat, env.getMinX.toFloat, env.getMaxY.toFloat)
          // for every region see what symbols intercept/are contained in
          val tokens = tokenSet.rTree.searchAll(minX, minY, maxX, maxY).map(treeEntry => treeEntry.value).toList
          // start at level 1 so need to subtract 1
          val phocVectorIndex = (split.levelNum-1) + split.splitNum
          tokens.foreach { t =>
            m.get(t.key) match
              case Some(oldPhoc) =>
                m.update(t.key, oldPhoc ::: (fill(phocVectorIndex - oldPhoc.length)(0) ::: (f(split, t) :: Nil)))
              case None => m + (t.key -> fill(phocVectorIndex)(0))
          }
        }
      }
      PartialPhoc(m.toMap)

  given standardExtract: ExtractPhoc[Geo2DTokenSet, Geo2DSplit] with
    override def extract(tokenSet: Geo2DTokenSet, splits: List[List[Geo2DSplit]]): PartialPhoc =
      val phocs = tokenSet.tokens.foldLeft(Map[String, List[Byte]]())((m, word) => {
        val phoc = splits.flatMap(level => level.map(split => TestMembership[Geo2DSplit, Geo2DToken].test(split, word)))
        // merge phocs
        m.get(word.key) match
          case Some(oldPhoc) => m + (word.key -> oldPhoc.zip(phoc).map((oldBit, newBit) => max(oldBit, newBit).toByte))
          case None => m + (word.key -> phoc)
      })
      PartialPhoc(phocs)

}