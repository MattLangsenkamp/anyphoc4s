package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.geo.*
import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DSpec, Geo2DTokenSet, HorzSpec, RectSpec, SVG, Source, Spec, TokenSet, VertSpec}
import org.dprl.anyphoc4s.splits.{EllipseSplit, Geo2DSplit, HorzSplit, Split, VertSplit, RectSplit}
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.parser.Tag


trait Visualize[A <: TokenSet, B <: Spec, C <: Source] {

  def visualize(tokenSet: A, levels: List[B], source: C): String
}
object Visualize {

  def apply[A <: TokenSet, B <: Spec, C <: Source](using ev: Visualize[A, B, C]): Visualize[A, B, C] = ev

  given svgVisualize: Visualize[Geo2DTokenSet, Geo2DSpec, SVG] with
    def visualize(tokenSet: Geo2DTokenSet, specs: List[Geo2DSpec], source: SVG): String =
      val htmlDoc = Jsoup.parse("<html><body></body></html>")
      htmlDoc.outputSettings().prettyPrint(true)
      val doc = htmlDoc.body()
      // append top level split
      val topLevelSplit = new Element(Tag.valueOf("div"), "")
      topLevelSplit.attr("style", "display: flex; flex-direction: row; justify-content: space-between")
      val html = Jsoup.parse(source.contents)
      val svg = html.body().child(0)
      svg.attr("style", "margin: auto;")
      svg.attr("width", "600px")
      svg.removeAttr("height")
      svg.append(tokenSet.tokenSetMeta.boundingBox.poly.toSVG(fillColor = "red"))
      topLevelSplit.appendChild(svg)
      doc.appendChild(topLevelSplit)


      // append spec level splits
      val specSplits = new Element(Tag.valueOf("div"), "")
      specSplits.attr("style", "display: flex; flex-direction: row; justify-content: space-evenly;")
      val listOfSpecSplits = specs.map {
        case s: VertSpec =>
          Expand[VertSpec, Geo2DTokenSet, VertSplit].expand(s, tokenSet)
        case s: HorzSpec =>
          Expand[HorzSpec, Geo2DTokenSet, HorzSplit].expand(s, tokenSet)
        case s: EllipseSpec =>
          Expand[EllipseSpec, Geo2DTokenSet, EllipseSplit].expand(s, tokenSet)
        case s: RectSpec =>
          Expand[RectSpec, Geo2DTokenSet, RectSplit].expand(s, tokenSet)
      }
      listOfSpecSplits.foreach { specSplit =>
        specSplits.append(VisualizeSpec[Geo2DTokenSet, Geo2DSplit, SVG].visualize(tokenSet, specSplit, source))
      }
      doc.appendChild(specSplits)
      htmlDoc.outputSettings().prettyPrint(true)
      htmlDoc.toString
}
