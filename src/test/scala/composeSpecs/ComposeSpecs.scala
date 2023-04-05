package composeSpecs

import org.dprl.anyphoc4s.{Compose, Tokenize, Visualize}
import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DRepr, Geo2DSpec, Geo2DTokenSet, Geo2DTokenSetSpec, HorzSpec, Phoc, SVG, VertSpec}

import java.io.{File, PrintWriter}
import scala.io.Source

class ComposeSpecs extends munit.FunSuite {

  val tokenizer: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] = Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet]

  def getPhoc(level: Int, svgName: String, visName: Option[String] = None): Phoc =
    val svg = SVG(Source.fromResource(svgName).mkString)
    val ellipseSpec = EllipseSpec(endLevel = level)
    val vertSpec = VertSpec(endLevel = level)
    val horzSpec = HorzSpec(endLevel = level)

    val specs = List(ellipseSpec, vertSpec, horzSpec)

    val tokenSet = tokenizer.convert(svg, Geo2DTokenSetSpec(repr = Geo2DRepr.BBOX))

    visName.foreach { s =>
      val str = Visualize[Geo2DTokenSet, Geo2DSpec, SVG].visualize(
        tokenSet,
        specs,
        svg
      )
      val writer = new PrintWriter(new File(s))
      writer.print(str)
      writer.close()
    }

    Compose[Geo2DTokenSet, Geo2DSpec].compose(specs)(tokenSet)


  test("L4 horizontal and vertical dominant svg ell, vert, horz") {
    val phoc = getPhoc(level = 4, svgName = "hv_dom.svg")

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      "𝑐" -> List(1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0),
      "𝑑" -> List(1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0),
      "-" -> List(1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      "𝑒" -> List(1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0),
      "𝑓" -> List(1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1),
      "𝑔" -> List(1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1),
      "+" -> List(1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0),
      "2" -> List(1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0),
      "4" -> List(1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0),
      "0" -> List(1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0),
      "9" -> List(1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0),
      "𝑏" -> List(1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1),
    )

    assertEquals(phoc.map, dTruth)
  }

}
