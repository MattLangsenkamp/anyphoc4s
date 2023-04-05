package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DSpec, Geo2DTokenSet, Geo2DTokenSetRTree2D, HorzSpec, PartialPhoc, Phoc, RectSpec, Spec, TokenSet, VertSpec}
import org.dprl.anyphoc4s.splits.{EllipseSplit, Geo2DSplit, HorzSplit, RectSplit, Split, VertSplit}


trait Compose[A <: TokenSet, B <: Spec] {
  def compose( specList: List[B], firstBit: Boolean = true)(tokenSet: A): Phoc
}

object Compose {
  def apply[A <: TokenSet, B <: Spec](implicit ev: Compose[A, B]): Compose[A, B] = ev

  private def mergePartialPhocs[A <: TokenSet](phocFuncList: List[A => PartialPhoc], firstBit: Boolean = true): (A => Phoc) = (tokenSet: A) => {
    phocFuncList
      .map(_ (tokenSet))
      .reduce(
        _ append _
      ).toPhoc(firstBit)
  }

  private def geo2DInstance[A <: Geo2DTokenSet](specList: List[Geo2DSpec], ext: ExtractPhoc[A, Geo2DSplit]): List[A => PartialPhoc] = {
    specList.map {
      case s: VertSpec =>
        (tc: A) => ext.extract(tc, Expand[VertSpec, Geo2DTokenSet, VertSplit].expand(s, tc))
      case s: HorzSpec =>
        (tc: A) => ext.extract(tc, Expand[HorzSpec, Geo2DTokenSet, HorzSplit].expand(s, tc))
      case s: EllipseSpec =>
        (tc: A) => ext.extract(tc, Expand[EllipseSpec, Geo2DTokenSet, EllipseSplit].expand(s, tc))
      case s: RectSpec =>
        (tc: A) => ext.extract(tc, Expand[RectSpec, Geo2DTokenSet, RectSplit].expand(s, tc))
    }
  }

  given Geo2DCompose: Compose[Geo2DTokenSet, Geo2DSpec] with
    override def compose(specList: List[Geo2DSpec], firstBit: Boolean)(tokenSet: Geo2DTokenSet): Phoc =
      val ext = ExtractPhoc[Geo2DTokenSet, Geo2DSplit]
      val tokenSetToPhocFunctions = geo2DInstance[Geo2DTokenSet](specList, ext)
      mergePartialPhocs(tokenSetToPhocFunctions, firstBit)(tokenSet)


  given Geo2DComposeRTree2D: Compose[Geo2DTokenSetRTree2D, Geo2DSpec] with
    override def compose(specList: List[Geo2DSpec], firstBit: Boolean)(tokenSet: Geo2DTokenSetRTree2D): Phoc =
      val ext = ExtractPhoc[Geo2DTokenSetRTree2D, Geo2DSplit]
      val tokenSetToPhocFunctions = geo2DInstance[Geo2DTokenSetRTree2D](specList, ext)
      mergePartialPhocs(tokenSetToPhocFunctions, firstBit)(tokenSet)
}


