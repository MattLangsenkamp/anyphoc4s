package ellipticalPhoc

import org.dprl.anyphoc4s.{Compose, Tokenize, Visualize}
import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DRepr, Geo2DSpec, Geo2DTokenSet, Geo2DTokenSetSpec, Phoc, SVG, Spec}
import org.dprl.anyphoc4s.splits.EllipseSplit

import java.io.{File, PrintWriter}
import scala.io.Source

class ellNoAngle extends munit.FunSuite {

  val tokenizer: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] = Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet]

  def getPhoc(level: Int, svgName: String, visName: Option[String] = None): Phoc =
    val svg = SVG(Source.fromResource(svgName).mkString)
    val vertPhoc = EllipseSpec(endLevel = level)
    val tokenSet = tokenizer.convert(svg, Geo2DTokenSetSpec(repr = Geo2DRepr.BBOX))

    visName.foreach { s =>
      val str = Visualize[Geo2DTokenSet, Geo2DSpec, SVG].visualize(
        tokenSet,
        List(EllipseSpec(endLevel = level)),
        svg
      )
      val writer = new PrintWriter(new File(s))
      writer.print(str)
      writer.close()
    }

    Compose[Geo2DTokenSet, Geo2DSpec].compose(List(vertPhoc))(tokenSet)

  test("L2 vertical dominant svg") {
    val phoc = getPhoc(level = 2, svgName = "v_dom.svg")

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎"-> List(1, 1, 1),
    "𝑐"-> List(1, 0, 1),
    "𝑑"-> List(1, 0, 1),
    "-"-> List(1, 1, 1),
    "𝑒"-> List(1, 0, 1),
    "𝑓"-> List(1, 0, 1),
    "𝑔"-> List(1, 1, 1),
    )

    assertEquals(phoc.map, dTruth)
  }

  test("L3 vertical dominant svg") {
    val phoc = getPhoc(level = 3, svgName = "v_dom.svg")

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎"-> List(1, 1, 1, 1, 1, 0),
    "𝑐"-> List(1, 0, 1, 0, 1, 0),
    "𝑑"-> List(1, 0, 1, 0, 0, 1),
    "-"-> List(1, 1, 1, 1, 1, 1),
    "𝑒"-> List(1, 0, 1, 0, 0, 1),
    "𝑓"-> List(1, 0, 1, 0, 1, 1),
    "𝑔"-> List(1, 1, 1, 1, 1, 0),
    )

    assertEquals(phoc.map, dTruth)
  }

  test("L4 vertical dominant svg") {
    val phoc = getPhoc(level = 4, svgName = "v_dom.svg")

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎"-> List(1, 1, 1, 1, 1, 0, 1, 1, 0, 0),
    "𝑐"-> List(1, 0, 1, 0, 1, 0, 0, 1, 1, 0),
    "𝑑"-> List(1, 0, 1, 0, 0, 1, 0, 0, 1, 1),
    "-"-> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    "𝑒"-> List(1, 0, 1, 0, 0, 1, 0, 0, 0, 1),
    "𝑓"-> List(1, 0, 1, 0, 1, 1, 0, 0, 1, 1),
    "𝑔"-> List(1, 1, 1, 1, 1, 0, 1, 1, 0, 0),
    )

    assertEquals(phoc.map, dTruth)
  }

  test("L2 horizontal dominant svg") {
    val phoc = getPhoc(level = 2, svgName = "h_dom.svg")

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎"-> List(1, 1, 1),
    "+"-> List(1, 1, 1),
    "𝑏"-> List(1, 1, 1),
    "𝑐"-> List(1, 0, 1),
    "𝑑"-> List(1, 0, 1),
    "𝑒"-> List(1, 0, 1),
    "𝑓"-> List(1, 1, 1),
    "𝑔"-> List(1, 1, 1),
    "ℎ"-> List(1, 1, 1),
    )

    assertEquals(phoc.map, dTruth)
  }

  test("L3 horizontal dominant svg") {
    val phoc = getPhoc(level = 3, svgName = "h_dom.svg")

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎"-> List(1, 1, 1, 1, 1, 0),
    "+"-> List(1, 1, 1, 1, 1, 1),
    "𝑏"-> List(1, 1, 1, 1, 1, 0),
    "𝑐"-> List(1, 0, 1, 0, 1, 1),
    "𝑑"-> List(1, 0, 1, 0, 1, 1),
    "𝑒"-> List(1, 0, 1, 0, 1, 1),
    "𝑓"-> List(1, 1, 1, 1, 1, 1),
    "𝑔"-> List(1, 1, 1, 1, 1, 0),
    "ℎ"-> List(1, 1, 1, 1, 1, 0),
    )

    assertEquals(phoc.map, dTruth)
  }

  test("L4 horizontal dominant svg") {
    val phoc = getPhoc(level = 4, svgName = "h_dom.svg")

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎"-> List(1, 1, 1, 1, 1, 0, 1, 1, 0, 0),
    "+"-> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    "𝑏"-> List(1, 1, 1, 1, 1, 0, 1, 1, 1, 0),
    "𝑐"-> List(1, 0, 1, 0, 1, 1, 0, 1, 1, 0),
    "𝑑"-> List(1, 0, 1, 0, 1, 1, 0, 1, 1, 1),
    "𝑒"-> List(1, 0, 1, 0, 1, 1, 0, 0, 1, 1),
    "𝑓"-> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
    "𝑔"-> List(1, 1, 1, 1, 1, 0, 1, 1, 1, 0),
    "ℎ"-> List(1, 1, 1, 1, 1, 0, 1, 1, 0, 0),
    )

    assertEquals(phoc.map, dTruth)
  }

  test("L2 horizontal-vertical dominant svg") {
    val phoc = getPhoc(level = 2, svgName = "hv_dom.svg")

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎"-> List(1, 1, 1),
    "𝑐"-> List(1, 1, 0),
    "𝑑"-> List(1, 0, 1),
    "-"-> List(1, 1, 1),
    "𝑒"-> List(1, 0, 1),
    "𝑓"-> List(1, 1, 1),
    "𝑔"-> List(1, 1, 1),
    "+"-> List(1, 0, 1),
    "2"-> List(1, 0, 1),
    "4"-> List(1, 0, 1),
    "0"-> List(1, 0, 1),
    "9"-> List(1, 0, 1),
    "𝑏"-> List(1, 1, 0),
    )

    assertEquals(phoc.map, dTruth)
  }

  test("L3 horizontal-vertical dominant svg") {
    val phoc = getPhoc(level = 3, svgName = "hv_dom.svg")

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎"-> List(1, 1, 1, 1, 1, 1),
    "𝑐"-> List(1, 1, 0, 1, 0, 0),
    "𝑑"-> List(1, 0, 1, 0, 1, 0),
    "-"-> List(1, 1, 1, 1, 1, 0),
    "𝑒"-> List(1, 0, 1, 0, 1, 0),
    "𝑓"-> List(1, 1, 1, 1, 1, 0),
    "𝑔"-> List(1, 1, 1, 1, 1, 0),
    "+"-> List(1, 0, 1, 0, 1, 1),
    "2"-> List(1, 0, 1, 0, 1, 1),
    "4"-> List(1, 0, 1, 0, 0, 1),
    "0"-> List(1, 0, 1, 0, 1, 1),
    "9"-> List(1, 0, 1, 0, 1, 0),
    "𝑏"-> List(1, 1, 0, 1, 0, 0),
    )

    assertEquals(phoc.map, dTruth)
  }

  test("L4 horizontal-vertical dominant svg") {
    val phoc = getPhoc(level = 4, svgName = "hv_dom.svg")

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎"-> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    "𝑐"-> List(1, 1, 0, 1, 0, 0, 1, 0, 0, 0),
    "𝑑"-> List(1, 0, 1, 0, 1, 0, 0, 1, 0, 0),
    "-"-> List(1, 1, 1, 1, 1, 0, 1, 1, 1, 0),
    "𝑒"-> List(1, 0, 1, 0, 1, 0, 0, 1, 0, 0),
    "𝑓"-> List(1, 1, 1, 1, 1, 0, 1, 1, 1, 0),
    "𝑔"-> List(1, 1, 1, 1, 1, 0, 1, 1, 0, 0),
    "+"-> List(1, 0, 1, 0, 1, 1, 0, 1, 1, 1),
    "2"-> List(1, 0, 1, 0, 1, 1, 0, 0, 1, 1),
    "4"-> List(1, 0, 1, 0, 0, 1, 0, 0, 1, 1),
    "0"-> List(1, 0, 1, 0, 1, 1, 0, 0, 1, 1),
    "9"-> List(1, 0, 1, 0, 1, 0, 0, 1, 1, 0),
    "𝑏"-> List(1, 1, 0, 1, 0, 0, 1, 0, 0, 0),
    )

    assertEquals(phoc.map, dTruth)
  }

  test("L4 single dominant svg") {
    val phoc = getPhoc(level = 4, svgName = "single.svg")

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎"-> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    )

    assertEquals(phoc.map, dTruth)
  }
}
