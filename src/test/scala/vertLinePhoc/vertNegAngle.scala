package vertLinePhoc

import org.dprl.anyphoc4s.{Compose, Tokenize, Visualize}
import org.dprl.anyphoc4s.model.{Geo2DRepr, Geo2DSpec, Geo2DTokenSet, Geo2DTokenSetSpec, Phoc, SVG, Spec, VertSpec}
import org.dprl.anyphoc4s.splits.VertSplit

import java.io.{File, PrintWriter}
import scala.io.Source

class vertNegAngle extends munit.FunSuite {

  val tokenizer: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] = Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet]

  def getPhoc(level: Int, svgName: String, angle: Float, visName: Option[String] = None): Phoc =
    val svg = SVG(Source.fromResource(svgName).mkString)
    val vertPhoc = VertSpec(endLevel = level, angle = angle)
    val tokenSet = tokenizer.convert(svg, Geo2DTokenSetSpec(repr = Geo2DRepr.BBOX))

    visName.foreach { s =>
      val str = Visualize[Geo2DTokenSet, Geo2DSpec, SVG].visualize(
        tokenSet,
        List(VertSpec(endLevel = level, angle=angle, name = "VertSpec")),
        svg
      )
      val writer = new PrintWriter(new File(s))
      writer.print(str)
      writer.close()
    }

    Compose[Geo2DTokenSet, Geo2DSpec].compose(List(vertPhoc))(tokenSet)

  test("L3 vertical dominant svg -3 angle") {
    val phoc = getPhoc(level = 3, svgName = "v_dom.svg", angle = -3f)

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎"-> List(1, 1, 1, 0, 1, 1),
      "𝑐"-> List(1, 1, 1, 0, 1, 0),
      "𝑑"-> List(1, 1, 1, 0, 1, 0),
      "-"-> List(1, 1, 1, 1, 1, 1),
      "𝑒"-> List(1, 1, 1, 0, 1, 0),
      "𝑓"-> List(1, 1, 1, 0, 1, 0),
      "𝑔"-> List(1, 1, 1, 1, 1, 0),
    )

    assertEquals(phoc.map, dTruth)
  }

  test("L3 horizontal dominant svg -3 angle") {
    val phoc = getPhoc(level = 3, svgName = "h_dom.svg", angle = -3f)

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List(1, 1, 0, 1, 0, 0),
      "+"-> List(1, 1, 1, 1, 1, 1),
      "𝑏"-> List(1, 1, 0, 1, 0, 0),
      "𝑐"-> List(1, 1, 0, 1, 0, 0),
      "𝑑"-> List(1, 1, 0, 0, 1, 0),
      "𝑒"->List(1, 0, 1, 0, 1, 0),
      "𝑓"-> List(1, 0, 1, 0, 0, 1),
      "𝑔"-> List(1, 0, 1, 0, 0, 1),
      "ℎ"-> List(1, 0, 1, 0, 0, 1),
    )

    assertEquals(phoc.map, dTruth)
  }

  test("L3 horizontal-vertical dominant svg -3 angle") {
    val phoc = getPhoc(level = 3, svgName = "hv_dom.svg", angle = -3f)

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List(1, 1, 1, 1, 1, 1),
      "𝑐" -> List(1, 1, 0, 1, 0, 0),
      "𝑑"-> List(1, 1, 0, 1, 0, 0),
      "-"-> List(1, 1, 1, 1, 0, 1),
      "𝑒"-> List(1, 1, 0, 1, 0, 0),
      "𝑓"-> List(1, 1, 1, 1, 0, 1),
      "𝑔"-> List(1, 1, 1, 1, 0, 1),
      "+"-> List(1, 1, 0, 1, 1, 0),
      "2"-> List(1, 1, 0, 0, 1, 0),
      "4"-> List(1, 0, 1, 0, 1, 0),
      "0"-> List(1, 0, 1, 0, 1, 1),
      "9"-> List(1, 0, 1, 0, 0, 1),
      "𝑏"-> List(1, 0, 1, 0, 0, 1),
    )

    assertEquals(phoc.map, dTruth)
  }

  test("L5 singular dominant svg -3 angle") {
    val phoc = getPhoc(level = 5, svgName = "single.svg", angle = -3f)

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    )

    assertEquals(phoc.map, dTruth)
  }

}
