package org.dprl.anyphoc4s

import org.dprl.*
import org.dprl.anyphoc4s.geo.BoundingBox
import org.dprl.anyphoc4s.geo.Geo.getCircleSteps
import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DMeta, Geo2DRepr, Geo2DSpec, Geo2DToken, Geo2DTokenSet, Geo2DTokenSetSpec, HorzSpec, PartialPhoc, RectSpec, SVG, Spec, VertSpec}
import org.dprl.anyphoc4s.splits.{Geo2DSplit, HorzSplit, RectSplit, VertSplit}
import org.jsoup.Jsoup

import java.io.{File, IOException, PrintWriter}
import scala.io.Source
import scala.xml.Elem

object Main {

  def main(args: Array[String]): Unit = {

    val svg = SVG(Source.fromResource("B362.svg").mkString)
    val vertPhoc = Spec.prep[Geo2DTokenSet, VertSpec, VertSplit](VertSpec(numLevels = 2, name = "VertSpec"))
    val horzPhoc = Spec.prep[Geo2DTokenSet, HorzSpec, HorzSplit](HorzSpec(numLevels = 1, name = "HorzSplit"))
    val tokenSet = Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet].convert(svg, Geo2DTokenSetSpec(repr = Geo2DRepr.BBOX, scale = 1.0))
    val phoc = ComposeSpecs.composeSpecs[Geo2DTokenSet](List(vertPhoc, horzPhoc))(tokenSet)
    println(phoc)
    val str = Visualize[Geo2DTokenSet, Geo2DSpec, SVG].visualize(
      tokenSet,
      List(VertSpec(numLevels = 3, name = "VertSpec")),
      svg
    )

    //println(str)
    val writer = new PrintWriter(new File("test.html"))
    writer.print(str)
    writer.close()
  }

  def time[R](block: => R)(message: String): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    val t = (t1 - t0) / 1e9
    if (t > 0.00001) println(s"Elapsed time to complete $message: " + t + "s")
    result
  }
}
