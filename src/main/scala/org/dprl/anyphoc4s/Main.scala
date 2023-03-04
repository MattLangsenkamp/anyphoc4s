package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.geo.BoundingBox
import org.dprl.anyphoc4s.splits.{HorzSplit, VertSplit}

import java.io.{File, IOException}

object Main {


  def main(args: Array[String]): Unit = {

    val vertPhoc = Spec.prep[Geo2DTokenSet, VertSpec, VertSplit](VertSpec(numLevels = 1, 0, name = "name"))
    val horzPhoc = Spec.prep[Geo2DTokenSet, HorzSpec, HorzSplit](HorzSpec(1, 0, name = "name"))

    val phoc = ComposeSpecs.composeSpecs[Geo2DTokenSet](List(vertPhoc, horzPhoc))(new Geo2DTokenSet(Geo2DMeta(BoundingBox(1, 1, 2, 2)), List(), None, None))
    println(phoc)

    for (c <- 0 until 1) {
      println(c)
    }
  }
}
