package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.geo.BoundingBox

trait TokenSetMeta 

case class Geo2DMeta(boundingBox: BoundingBox) extends TokenSetMeta
