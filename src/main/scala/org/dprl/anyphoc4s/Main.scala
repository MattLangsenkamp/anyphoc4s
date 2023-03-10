package org.dprl.anyphoc4s
import org.dprl.*
import org.dprl.anyphoc4s.geo.BoundingBox
import org.dprl.anyphoc4s.model.{Geo2DMeta, Geo2DTokenSet, HorzSpec, Spec, VertSpec}
import org.dprl.anyphoc4s.splits.{Geo2DSplit, HorzSplit, VertSplit}

import java.io.{File, IOException}

object Main {


  def main(args: Array[String]): Unit = {

    println((1 to 1 by 1).toArray.mkString("Array(", ", ", ")"))

    val vertPhoc = Spec.prep[Geo2DTokenSet, VertSpec, VertSplit](VertSpec(numLevels = 1, name = "name"))
    val horzPhoc = Spec.prep[Geo2DTokenSet, HorzSpec, HorzSplit](HorzSpec(numLevels = 1, name = "name"))

    val phoc = ComposeSpecs.composeSpecs[Geo2DTokenSet](List(vertPhoc, horzPhoc))(new Geo2DTokenSet(Geo2DMeta(BoundingBox(1, 1, 2, 2)), List()))
    println(phoc)
  }
}
