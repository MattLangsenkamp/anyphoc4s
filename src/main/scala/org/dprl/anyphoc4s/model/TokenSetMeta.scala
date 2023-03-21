package org.dprl.anyphoc4s.model

import org.dprl.anyphoc4s.model.TokenSetSpec
import org.dprl.anyphoc4s.geo.BoundingBox

trait TokenSetMeta {
  def spec: TokenSetSpec
}

case class Geo2DMeta(boundingBox: BoundingBox, spec: Geo2DTokenSetSpec) extends TokenSetMeta
