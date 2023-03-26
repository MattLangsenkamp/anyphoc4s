package horzLinePhoc

import org.dprl.anyphoc4s.{ComposeSpecs, Tokenize, Visualize}
import org.dprl.anyphoc4s.model.{Geo2DRepr, Geo2DSpec, Geo2DTokenSet, Geo2DTokenSetSpec, HorzSpec, Phoc, SVG, Spec, VertSpec}
import org.dprl.anyphoc4s.splits.{HorzSplit, VertSplit}

import java.io.{File, PrintWriter}
import scala.io.Source

class negAngle extends munit.FunSuite {

  val tokenizer: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] = Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet]

  def getPhoc(level: Int, svgName: String, angle: Float, visName: Option[String] = None): Phoc =
    val svg = SVG(Source.fromResource(svgName).mkString)
    val vertPhoc = Spec.prep[Geo2DTokenSet, HorzSpec, HorzSplit](HorzSpec(numLevels = level, angle = angle, name = "HorzSpec"))
    val tokenSet = tokenizer.convert(svg, Geo2DTokenSetSpec(repr = Geo2DRepr.BBOX))

    visName.foreach { s =>
      val str = Visualize[Geo2DTokenSet, Geo2DSpec, SVG].visualize(
        tokenSet,
        List(HorzSpec(numLevels = level, angle = angle, name = "HorzSpec")),
        svg
      )
      val writer = new PrintWriter(new File(s))
      writer.print(str)
      writer.close()
    }

    ComposeSpecs.composeSpecs[Geo2DTokenSet](List(vertPhoc))(tokenSet)

  test("L5 vertical dominant svg -3 angle") {
    val phoc = getPhoc(level = 5, svgName = "v_dom.svg", angle = -3f, Some("pp.html"))

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List(1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0),
      "𝑐" -> List(1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0),
      "𝑑" -> List(1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
      "-" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
      "𝑒" -> List(1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
      "𝑓" -> List(1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0),
      "𝑔" -> List(1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1),
    )

    assertEquals(phoc.map, dTruth)
  }

}
