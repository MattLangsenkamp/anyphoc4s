package org.dprl.anyphoc4s.splits

import org.dprl.anyphoc4s.*
import org.locationtech.jts.geom.*
import org.locationtech.jts.geom.util.AffineTransformation


trait Split

trait Geo2DSplit extends Split {
  def phocRegion: Polygon
}

