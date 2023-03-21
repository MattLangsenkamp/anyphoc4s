package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.model.{Geo2DToken, Geo2DTokenSet, SVG, Source, Spec, TokenSet}
import org.dprl.anyphoc4s.splits.{Geo2DSplit, Split}
import org.jcp.xml
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.parser.Tag
import scala.util.{Failure, Success, Try}
import scala.xml.Node

trait VisualizeSpec[A <: TokenSet, B <: Split, C <: Source] {
  def visualize(tokenSet: A, levels: List[List[B]], source: C): String
}

object VisualizeSpec {

  def apply[A <: TokenSet, B <: Split, C <: Source](using ev: VisualizeSpec[A, B, C]): VisualizeSpec[A, B, C] = ev

  given svgVis: VisualizeSpec[Geo2DTokenSet, Geo2DSplit, SVG] with
    override def visualize(tokenSet: Geo2DTokenSet, levels: List[List[Geo2DSplit]], source: SVG): String =
      val html = Jsoup.parse(source.contents)
      html.outputSettings().prettyPrint(true)
      val svg = html.body().child(0)
      svg.attr("style", "margin-top: 5px;")

      val specDiv = new Element(Tag.valueOf("div"), "")
      specDiv.attr("style", "display: flex; flex-direction: column;")
      levels.foreach { l =>
        val levelSvg = svg.clone()
        levelSvg.attr("width", "600px")
        levelSvg.removeAttr("height")
        l.zipWithIndex.foreach { (s, i) =>
          levelSvg.append(VisualizeSplit[Geo2DSplit].visualize(s, i))
        }
        tokenSet.tokens.foreach { t =>
          val r = VisualizeToken[Geo2DToken].visualize(t)
          levelSvg.append(r)
        }
        specDiv.appendChild(levelSvg)
      }

      specDiv.toString
}