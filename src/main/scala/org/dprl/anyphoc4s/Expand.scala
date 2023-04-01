package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DTokenSet, HorzSpec, Spec, TokenSet, VertSpec, RectSpec}
import org.dprl.anyphoc4s.splits.Split
import org.dprl.anyphoc4s.splits.{EllipseSplit, HorzSplit, VertSplit, RectSplit}
import math.max

trait Expand[A <: Spec, B <: TokenSet, C <: Split] {
  def expand(spec: A, tokenSet: B): List[List[C]]
}

object Expand {
  def apply[A <: Spec, B <: TokenSet, C <: Split](implicit ev: Expand[A, B, C]): Expand[A, B, C] = ev

  private def instance[A <: Spec, B <: TokenSet, C <: Split](f: (Int, Int, A, B) => C): Expand[A, B, C] =
    (spec: A, tokenSet: B) => (max( spec.startLevel, 2 ) to spec.endLevel by (spec.skipStep + 1))
      .map { l =>
        (0 until l)
          .map( s =>
            f( l, s, spec, tokenSet )
          ).toList
      }.toList

  given vertExpand: Expand[VertSpec, Geo2DTokenSet, VertSplit] =
    instance( (l: Int, n: Int, s: VertSpec, b: Geo2DTokenSet) => VertSplit( l, n, s, b ) )

  given horzExpand: Expand[HorzSpec, Geo2DTokenSet, HorzSplit] =
    instance( (l: Int, n: Int, s: HorzSpec, b: Geo2DTokenSet) => HorzSplit( l, n, s, b ) )

  given ellipseExpand: Expand[EllipseSpec, Geo2DTokenSet, EllipseSplit] =
    instance( (l: Int, n: Int, s: EllipseSpec, b: Geo2DTokenSet) => EllipseSplit( l, n, s, b ) )

  given rectExpand: Expand[RectSpec, Geo2DTokenSet, RectSplit] =
    instance( (l: Int, n: Int, s: RectSpec, b: Geo2DTokenSet) => RectSplit( l, n, s, b ) )
}
