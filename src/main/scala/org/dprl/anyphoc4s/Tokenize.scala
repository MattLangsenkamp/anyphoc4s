package org.dprl.anyphoc4s

import collection.mutable.{ListBuffer, Seq}
import org.dprl.anyphoc4s.geo.BoundingBox
import org.dprl.anyphoc4s.model.{Geo2DMeta, Geo2DRepr, Geo2DToken, Geo2DTokenSet, Geo2DTokenSetSpec, SVG, Source, Spec, TokenSet, TokenSetSpec}
import org.dprl.svgbounds.model.SvgType
import org.dprl.svgbounds.{BoundOps, PathParse, TransformParse}
import org.dprl.svgbounds.model.SvgType.{Bounds, Path, Rect}
import org.dprl.svgbounds.model.Transformation.Matrix
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.NodeFilter
import math.{max, min}
import scala.collection.mutable
import scala.util.Try
import scala.xml.Node

trait Tokenize[A <: Source, B <: TokenSetSpec, C <: TokenSet] {
  def convert(source: A, spec: B): C
}

object Tokenize {

  def apply[A <: Source, B <: TokenSetSpec, C <: TokenSet](implicit ev: Tokenize[A, B, C]): Tokenize[A, B, C] = ev

  given svgMathConverter: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] with
    override def convert(source: SVG, spec: Geo2DTokenSetSpec): Geo2DTokenSet =
      var minX = Double.MaxValue
      var minY = Double.MaxValue
      var maxX = Double.MinValue
      var maxY = Double.MinValue
      var bounds: Bounds = SvgType.newBounds
      val mutList = new ListBuffer[Geo2DToken]()
      val initMatrix: Matrix = Matrix(1, 0, 0, 1, 0, 0)

      def parse(el: Element, matrix: Matrix): Unit =
        el.children().forEach { e =>
          e.tagName() match
            case "path" =>
              val nMatrix = potentiallyTransformMatrix(e, matrix)
              parsePath(e, nMatrix).foreach(token => mutList.addOne(token))
            case "g" =>
              val nMatrix = potentiallyTransformMatrix(e, matrix)
              parse(e, nMatrix)
            case "rect" =>
              val nMatrix = potentiallyTransformMatrix(e, matrix)
              parseRect(e, nMatrix).foreach(mutList.addOne)
            case "svg" =>
            // not yet handling recursive SVGs
            case "text" =>
            // we will not handle text as we need the glyph data to get bounding boxes
            case "line" =>
              // may want to ignore lines later. They are lines that cannot be seen but are used in matrix operations
              val nMatrix = potentiallyTransformMatrix(e, matrix)
              mutList.addOne(parseLine(e, nMatrix))
            case "polygon" =>
              // The information encoded in the polygon is not helpful, its just random boxes
              // val nMatrix = potentiallyTransformMatrix(e, matrix)
              // parsePolygon(e, nMatrix).foreach(token => mutList.addOne(token))
            case _ => println(s"unhandled tag type ${e.tagName()}")
        }

      def potentiallyTransformMatrix(el: Element, localMatrix: Matrix): Matrix =
        if (el.attributes().hasKey("transform"))
          TransformParse.transformList.parse(el.attributes().get("transform")) match
            case Left(_) =>
              println("failed to parse transform matrix")
              localMatrix
            case Right(value) =>
              localMatrix * BoundOps.CollapseTransforms(value._2)
        else
          localMatrix

      def parseRect(el: Element, matrix: Matrix): Option[Geo2DToken] =
        if (el.parent().hasAttr("data-mml-node") && el.parent().attr("data-mml-node") == "mfrac")
          val x = el.attr("x").toDouble
          val y = el.attr("y").toDouble
          val width = el.attr("width").toDouble
          val height = el.attr("height").toDouble
          val rect = Rect(x, y, width, height)
          val nRect = rect * matrix
          minX = min(nRect.x, minX);
          minY = min(nRect.y, minY)
          maxX = max(nRect.width + nRect.x, maxX);
          maxY = max(nRect.height + nRect.y, maxY)
          bounds = bounds + Bounds(nRect.x, nRect.y, nRect.x + nRect.width, nRect.y + nRect.height)
          Some(Geo2DToken(
            "-",
            BoundingBox(nRect.x, nRect.y, nRect.x + nRect.width, nRect.y + nRect.height, spec.scale),
            spec.repr)
          )
        else
          None

      def parsePath(el: Element, matrix: Matrix): Option[Geo2DToken] =
        val label = if (el.hasAttr("data-c"))
          val dataCCode = el.attr("data-c").toLowerCase
          String.valueOf(Character.toChars(Integer.parseInt(dataCCode, 16)))
        else "<UNK>"
        val d = el.attr("d")
        PathParse.svgPath.parse(d) match
          case Left(_) =>
            if (d != "")
              println(f"error parsing path $d")
            None
          case Right(value) =>
            val bBefore = BoundOps.getBounds(Path(value._2) * matrix)
            val b = accountForSquareRoot(el, label, bBefore, matrix)
            minX = min(b.xMin, minX);
            minY = min(b.yMin, minY)
            maxX = max(b.xMax, maxX);
            maxY = max(b.yMax, maxY)
            bounds = bounds + b
            Some(Geo2DToken(label, BoundingBox(b.xMin, b.yMin, b.xMax, b.yMax, spec.scale), spec.repr))

      def accountForSquareRoot(el: Element, label: String, b: Bounds, matrix: Matrix): Bounds =
        val next = el.parent().nextElementSibling()

        if (next != null && label == "√" && next.nodeName() == "rect")
          val x = next.attr("x").toDouble
          val y = next.attr("y").toDouble
          val width = math.max(next.attr("width").toDouble, 0.0001)
          val height = next.attr("height").toDouble
          val rect = Rect(x, y, width, height)
          val transformedRect = rect * matrix
          val n = b.copy(xMax = b.xMax + transformedRect.width)
          n
        else
          b

      def parsePolygon(el: Element, matrix: Matrix): Option[Geo2DToken] =
        val points = el.attr("points")
        PathParse.coordinatePairSequence.parse(points) match
          case Left(_) => None
          case Right(value) =>
            val b = value._2.foldLeft(
              Bounds(Double.MaxValue, Double.MaxValue, Double.MinValue, Double.MinValue)
            ) { (bounds, point) =>
              bounds + point
            }
            val rect = Rect(b.xMin, b.yMin, b.xMax-b.yMax, b.yMax-b.yMin) * matrix
            Some(Geo2DToken(
                "polygon",
                BoundingBox(rect.x, rect.y, rect.width+rect.x, rect.height+rect.y, spec.scale),
                spec.repr))

      def parseLine(el: Element, matrix: Matrix): Geo2DToken =
        var minX = el.attr("x1").toDouble
        var minY = el.attr("y1").toDouble
        var maxX = el.attr("x2").toDouble
        var maxY = el.attr("y2").toDouble

        if (maxX == minX)
          maxX = maxX + 0.0001
          minX = minX - 0.0001

        if (maxY == minY)
          maxY = maxY + 0.0001
          minY = minY - 0.0001

        val rect = Rect(minX, minY, maxX - minX, maxY - minY) * matrix

        val lineType = el.attr("data-line")
        Geo2DToken(
          f"line-$lineType",
          BoundingBox(rect.x, rect.y, rect.x + rect.width, rect.y + rect.height, spec.scale),
          spec.repr)

      val html = Jsoup.parse(source.contents)
      html.outputSettings().prettyPrint(true)
      val svg = html.body().getElementsByTag("svg").first()
      parse(svg, initMatrix)

      new Geo2DTokenSet(Geo2DMeta(BoundingBox(minX, minY, maxX, maxY), spec), mutList.toList)

}