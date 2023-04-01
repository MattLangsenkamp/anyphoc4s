package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DSpec, Geo2DTokenSet, HorzSpec, PartialPhoc, Phoc, RectSpec, Spec, TokenSet, VertSpec}
import org.dprl.anyphoc4s.splits.{EllipseSplit, HorzSplit, RectSplit, Split, VertSplit}


trait Compose[A <: TokenSet, B <: Spec] {
  def compose( specList: List[B], firstBit: Boolean = true)(tokenSet: A): Phoc
}

object Compose {
  def apply[A <: TokenSet, B <: Spec](implicit ev: Compose[A, B]): Compose[A, B] = ev

  private def instance[A <: TokenSet](phocFuncList: List[A => PartialPhoc], firstBit: Boolean = true): (A => Phoc) = (tokenSet: A) => {
    phocFuncList
      .map(_ (tokenSet))
      .reduce(
        _ append _
      ).toPhoc(firstBit)
  }

  given Geo2DCompose: Compose[Geo2DTokenSet, Geo2DSpec] with
    override def compose(specList: List[Geo2DSpec], firstBit: Boolean)(tokenSet: Geo2DTokenSet): Phoc =
      val tokenSetToPhocFunctions = specList.map {
        case s: VertSpec => Spec.prep[Geo2DTokenSet, VertSpec, VertSplit](s)
        case s: HorzSpec => Spec.prep[Geo2DTokenSet, HorzSpec, HorzSplit](s)
        case s: EllipseSpec => Spec.prep[Geo2DTokenSet, EllipseSpec, EllipseSplit](s)
        case s: RectSpec => Spec.prep[Geo2DTokenSet, RectSpec, RectSplit](s)
      }

      instance(tokenSetToPhocFunctions, firstBit)(tokenSet)
}

/*object ComposeSpecs {
  def composeSpecs[A <: TokenSet](phocFuncList: List[A => PartialPhoc], firstBit: Boolean = true): (A => Phoc) = (tokenSet: A) => {
    phocFuncList
      .map(_ (tokenSet))
      .reduce(
        _ append _
      ).toPhoc(firstBit)
  }
}*/
