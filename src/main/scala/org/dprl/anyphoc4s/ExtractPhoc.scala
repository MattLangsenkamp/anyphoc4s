package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.splits.{Geo2DSplit, HorzSplit, Split, VertSplit}

trait ExtractPhoc[A <: TokenSet, B <: Split] {
  def extract(tokenSet: A, splits: List[List[B]]): PartialPhoc
}

object ExtractPhoc {
  def apply[A <: TokenSet, B <: Split](implicit ev: ExtractPhoc[A, B]): ExtractPhoc[A, B] = ev

  given standardExtract: ExtractPhoc[Geo2DTokenSet, Geo2DSplit] with
    override def extract(tokenSet: Geo2DTokenSet, splits: List[List[Geo2DSplit]]): PartialPhoc = {
      val phocs = tokenSet.tokens.foldLeft(Map[String, List[Byte]]())( (m, word) => {
        val phoc = splits.flatMap(level => level.map(split => TestMembership[Geo2DSplit, Geo2DToken].test(split, word)))
        m + (word.key -> phoc)
      })
      PartialPhoc(phocs)
    }
    PartialPhoc(Map(""-> List()))

  given quadTreeExtract: ExtractPhoc[Geo2DTokenSet, VertSplit] with
    override def extract(tokenSet: Geo2DTokenSet, splits: List[List[VertSplit]]): PartialPhoc =
      PartialPhoc(Map(""-> List()))

  given rTree2DExtract: ExtractPhoc[Geo2DTokenSet, HorzSplit] with
    override def extract(tokenSet: Geo2DTokenSet, splits: List[List[HorzSplit]]): PartialPhoc =
      PartialPhoc(Map(""-> List()))
}