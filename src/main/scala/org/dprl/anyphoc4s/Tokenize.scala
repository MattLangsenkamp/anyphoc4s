package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.geo.{BoundingBox, SVG, Source}
import org.dprl.anyphoc4s.model.{Geo2DMeta, Geo2DTokenSet, Geo2DTokenSetSpec, HorzSpec, Spec, TokenSet, TokenSetSpec}

trait Tokenize[A <: Source, B <: TokenSetSpec, C <: TokenSet] {
  def convert(source: A, spec: B): C
}

object Tokenize {

  def apply[A <: Source, B <: TokenSetSpec, C<: TokenSet](implicit ev: Tokenize[A, B, C]): Tokenize[A, B, C] = ev

  given svgConverter: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] with
    override def convert(source: SVG, spec: Geo2DTokenSetSpec): Geo2DTokenSet = new Geo2DTokenSet(Geo2DMeta(BoundingBox(1,1,2,2)), List())

}