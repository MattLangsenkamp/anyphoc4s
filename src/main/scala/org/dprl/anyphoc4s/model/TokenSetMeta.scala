package org.dprl.anyphoc4s.model

import org.dprl.anyphoc4s.model.TokenSetMeta
import org.dprl.anyphoc4s.geo.BoundingBox

trait TokenSetMeta 

case class Geo2DMeta(boundingBox: BoundingBox) extends TokenSetMeta
