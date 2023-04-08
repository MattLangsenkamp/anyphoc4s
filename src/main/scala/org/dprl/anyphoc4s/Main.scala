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


    /*import com.github.plokhotnyuk.rtree2d.core._
    import EuclideanPlane._

    val box1 = entry(0f, 0f, 2.5f, 5f, "Box 1")
    val box2 = entry(2.5f, 0f, 5f, 5f, "Box 2")


    val entries = Seq(box1, box2)

    val rtree = RTree(entries)

    println(rtree.searchAll(2f, 2f, 3f, 3f).map(_.value))
    println(rtree.searchAll(4f, 0f, 5f, 1f).map(_.value))

    println((List(1,2,3) ::: (List(4)) ::: (1 :: Nil) ))*/


    val svg = SVG(Source.fromResource("polygonExample2.svg").mkString)
    val vertSpec = VertSpec(endLevel = 4)
    val horzSpec = HorzSpec(endLevel = 3)
    val rectSpec = RectSpec(endLevel = 4)
    val ellipseSpec = EllipseSpec(endLevel = 5)
    val tokenSet = Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet].convert(
      svg, Geo2DTokenSetSpec(repr = Geo2DRepr.BBOX))
    val specList = List(vertSpec, rectSpec, horzSpec, ellipseSpec)
    val phoc = Compose[Geo2DTokenSet, Geo2DSpec].compose(specList)(tokenSet)
    println(phoc)
    println(phoc.map.toSeq.map(_._1))
    val str = Visualize[Geo2DTokenSet, Geo2DSpec, SVG].visualize(
      tokenSet,
      specList,
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
