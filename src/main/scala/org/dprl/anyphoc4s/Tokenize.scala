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

  given svgConverter: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] with
    override def convert(source: SVG, spec: Geo2DTokenSetSpec): Geo2DTokenSet =
      var minX = Double.MaxValue
      var minY = Double.MaxValue
      var maxX = Double.MinValue
      var maxY = Double.MinValue
      var bounds: Bounds = SvgType.newBounds
      val mutList = new ListBuffer[Geo2DToken]
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
              mutList.addOne(parseRect(e, nMatrix))
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

      def parseRect(el: Element, matrix: Matrix): Geo2DToken =
        val x = el.attr("x").toDouble
        val y = el.attr("y").toDouble
        val width = el.attr("width").toDouble
        val height = el.attr("height").toDouble
        val rect = Rect(x, y, width, height)
        val nRect = rect * matrix
        minX = min(nRect.x, minX); minY = min(nRect.y, minY)
        maxX = max(nRect.width+nRect.x, maxX); maxY = max(nRect.height+nRect.y, maxY)
        bounds = bounds + Bounds(nRect.x, nRect.y, nRect.x + nRect.width, nRect.y + nRect.height)
        Geo2DToken("-", BoundingBox(nRect.x, nRect.y, nRect.x + nRect.width, nRect.y + nRect.height), spec.repr)

      def parsePath(el: Element, matrix: Matrix): Option[Geo2DToken] =
        val label = if (el.hasAttr("data-c"))
          Integer.parseInt(el.attr("data-c"), 16).toChar.toString
        else "<UNK>"
        val d = el.attr("d")
        PathParse.svgPath.parse(d) match
          case Left(_) =>
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
            Some(Geo2DToken(label, BoundingBox(b.xMin, b.yMin, b.xMax, b.yMax), spec.repr))

      def accountForSquareRoot(el: Element, label: String, b: Bounds, matrix: Matrix): Bounds =
        if (el.nextElementSibling() != null && label == "√" && el.nextElementSibling.nodeName() == "rect")
          val x = el.nextElementSibling.attr("x").toDouble
          val y = el.nextElementSibling.attr("y").toDouble
          val width = math.max(el.nextElementSibling.attr("width").toDouble, 0.0001)
          val height = el.nextElementSibling.attr("height").toDouble
          val rect = Rect(x, y, width, height)
          val transformedRect = rect * matrix
          val n = b.copy(xMax = transformedRect.width - transformedRect.x)
          n
        else
          b

      val html = Jsoup.parse(source.contents)
      html.outputSettings().prettyPrint(true)
      val svg = html.body().child(0)
      parse(svg, initMatrix)

      new Geo2DTokenSet(Geo2DMeta(BoundingBox(minX, minY, maxX, maxY), Geo2DTokenSetSpec()), mutList.toList)
}