package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DToken, Geo2DTokenSet, PartialPhoc, Token, TokenSet}
import org.dprl.anyphoc4s.splits.{EllipseSplit, Geo2DSplit, HorzSplit, Split, VertSplit}
import Math.max

trait ExtractPhoc[A <: TokenSet, B <: Split] {
  def extract(tokenSet: A, splits: List[List[B]]): PartialPhoc
}

object ExtractPhoc {
  def apply[A <: TokenSet, B <: Split](implicit ev: ExtractPhoc[A, B]): ExtractPhoc[A, B] = ev

  private def standardInstance[A <: TokenSet, B <: Split](tokenSet: A, splits: List[List[B]], f: (B, Geo2DToken) => Byte): PartialPhoc =
    val phocs = tokenSet.tokens.foldLeft(Map[String, List[Byte]]())((m, word) => {
      val phoc = splits.flatMap(level => level.map(split => f(split, word.asInstanceOf[Geo2DToken])))
      // merge phocs
      m.get(word.key) match
        case Some(oldPhoc) => m + (word.key -> oldPhoc.zip(phoc).map((oldBit, newBit) => max(oldBit, newBit).toByte))
        case None => m + (word.key -> phoc)

    })
    PartialPhoc(phocs)

  given standardVertExtract: ExtractPhoc[Geo2DTokenSet, VertSplit] with
    override def extract(tokenSet: Geo2DTokenSet, splits: List[List[VertSplit]]): PartialPhoc =
      standardInstance[Geo2DTokenSet, VertSplit](tokenSet, splits, TestMembership[Geo2DSplit, Geo2DToken].test)

  given standardHorzExtract: ExtractPhoc[Geo2DTokenSet, HorzSplit] with
    override def extract(tokenSet: Geo2DTokenSet, splits: List[List[HorzSplit]]): PartialPhoc =
      standardInstance[Geo2DTokenSet, HorzSplit](tokenSet, splits, TestMembership[Geo2DSplit, Geo2DToken].test)

  given standardEllipseExtract: ExtractPhoc[Geo2DTokenSet, EllipseSplit] with
    override def extract(tokenSet: Geo2DTokenSet, splits: List[List[EllipseSplit]]): PartialPhoc =
      standardInstance[Geo2DTokenSet, EllipseSplit](tokenSet, splits, TestMembership[Geo2DSplit, Geo2DToken].test)

  /*given quadTreeExtract: ExtractPhoc[Geo2DTokenSet, Geo2DSplit] with
    override def extract(tokenSet: Geo2DTokenSet, splits: List[List[VertSplit]]): PartialPhoc =
      PartialPhoc(Map(""-> List()))

  given rTree2DExtract: ExtractPhoc[Geo2DTokenSet, Geo2DSplit] with
    override def extract(tokenSet: Geo2DTokenSet, splits: List[List[HorzSplit]]): PartialPhoc =
      PartialPhoc(Map(""-> List()))*/
}