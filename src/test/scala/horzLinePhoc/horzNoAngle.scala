package horzLinePhoc

import org.dprl.anyphoc4s.{ComposeSpecs, Tokenize, Visualize}
import org.dprl.anyphoc4s.model.{Geo2DRepr, Geo2DSpec, Geo2DTokenSet, Geo2DTokenSetSpec, HorzSpec, Phoc, SVG, Spec, VertSpec}
import org.dprl.anyphoc4s.splits.{HorzSplit, VertSplit}

import java.io.{File, PrintWriter}
import scala.io.Source

class horzNoAngle extends munit.FunSuite {

  val tokenizer: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] = Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet]

  def getPhoc(level: Int, svgName: String, visName: Option[String] = None): Phoc =
    val svg = SVG(Source.fromResource(svgName).mkString)
    val vertPhoc = Spec.prep[Geo2DTokenSet, HorzSpec, HorzSplit](HorzSpec(numLevels = level, name = "VertSpec"))
    val tokenSet = tokenizer.convert(svg, Geo2DTokenSetSpec(repr = Geo2DRepr.BBOX))

    visName.foreach { s =>
      val str = Visualize[Geo2DTokenSet, Geo2DSpec, SVG].visualize(
        tokenSet,
        List(HorzSpec(numLevels = level, name = "HorzSpec")),
        svg
      )
      val writer = new PrintWriter(new File(s))
      writer.print(str)
      writer.close()
    }

    ComposeSpecs.composeSpecs[Geo2DTokenSet](List(vertPhoc))(tokenSet)

  test("L2 vertical dominant svg") {

    val phoc = getPhoc(2, "v_dom.svg")

    val dTruth = Map(
      "𝑎" -> List(1, 1, 0),
      "𝑐" -> List(1, 1, 0),
      "𝑑" -> List(1, 1, 0),
      "-" -> List(1, 1, 1),
      "𝑒" -> List(1, 0, 1),
      "𝑓" -> List(1, 0, 1),
      "𝑔" -> List(1, 0, 1),
    )

    assert(phoc.map == dTruth)
  }

  test("L3 vertical dominant svg") {
    val phoc = getPhoc(3, "v_dom.svg")

    val dTruth = Map(
      "𝑎" -> List(1, 1, 0, 1, 0, 0),
      "𝑐" -> List(1, 1, 0, 1, 0, 0),
      "𝑑" -> List(1, 1, 0, 1, 1, 0),
      "-" -> List(1, 1, 1, 1, 1, 1),
      "𝑒" -> List(1, 0, 1, 0, 1, 0),
      "𝑓" -> List(1, 0, 1, 0, 1, 1),
      "𝑔" -> List(1, 0, 1, 0, 0, 1),
    )

    assert(phoc.map == dTruth)
  }

  test("L4 vertical dominant svg") {
    val phoc = getPhoc(4, "v_dom.svg")

    val dTruth = Map(
      "𝑎" -> List(1, 1, 0, 1, 0, 0, 1, 0, 0, 0),
      "𝑐" -> List(1, 1, 0, 1, 0, 0, 1, 0, 0, 0),
      "𝑑" -> List(1, 1, 0, 1, 1, 0, 0, 1, 0, 0),
      "-" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      "𝑒" -> List(1, 0, 1, 0, 1, 0, 0, 0, 1, 0),
      "𝑓" -> List(1, 0, 1, 0, 1, 1, 0, 0, 1, 1),
      "𝑔" -> List(1, 0, 1, 0, 0, 1, 0, 0, 0, 1),
    )

    assert(phoc.map == dTruth)
  }

  test("L5 vertical dominant svg") {
    val phoc = getPhoc(5, "v_dom.svg", Some("yea.html"))

    val dTruth = Map(
      "𝑎" -> List(1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0),
      "𝑐" -> List(1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0),
      "𝑑" -> List(1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
      "-" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
      "𝑒" -> List(1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
      "𝑓" -> List(1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0),
      "𝑔" -> List(1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1),
    )

    assert(phoc.map == dTruth)
  }

  test("L2 horizontal dominant svg") {
    val phoc = getPhoc(2, "h_dom.svg")

    val dTruth = Map(
      "𝑎" -> List(1, 1, 1),
      "+" -> List(1, 1, 1),
      "𝑏" -> List(1, 1, 1),
      "𝑐" -> List(1, 1, 1),
      "𝑑" -> List(1, 1, 1),
      "𝑒" -> List(1, 1, 1),
      "𝑓" -> List(1, 1, 1),
      "𝑔" -> List(1, 1, 1),
      "ℎ" -> List(1, 1, 1),
    )

    assert(phoc.map == dTruth)
  }

  test("L3 horizontal dominant svg") {
    val phoc = getPhoc(3, "h_dom.svg")

    val dTruth = Map(
      "𝑎" -> List(1, 1, 1, 1, 1, 1),
      "+" -> List(1, 1, 1, 1, 1, 1),
      "𝑏" -> List(1, 1, 1, 1, 1, 1),
      "𝑐" -> List(1, 1, 1, 1, 1, 1),
      "𝑑" -> List(1, 1, 1, 1, 1, 1),
      "𝑒" -> List(1, 1, 1, 1, 1, 1),
      "𝑓" -> List(1, 1, 1, 1, 1, 1),
      "𝑔" -> List(1, 1, 1, 1, 1, 1),
      "ℎ" -> List(1, 1, 1, 1, 1, 1),
    )

    assert(phoc.map == dTruth)
  }

  test("L4 horizontal dominant svg") {
    val phoc = getPhoc(4, "h_dom.svg")

    val dTruth = Map(
      "𝑎" -> List(1, 1, 1, 1, 1, 1, 0, 1, 1, 1),
      "+" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      "𝑏" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      "𝑐" -> List(1, 1, 1, 1, 1, 1, 0, 1, 1, 1),
      "𝑑" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      "𝑒" -> List(1, 1, 1, 1, 1, 1, 0, 1, 1, 1),
      "𝑓" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      "𝑔" -> List(1, 1, 1, 1, 1, 1, 0, 1, 1, 1),
      "ℎ" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    )

    assert(phoc.map == dTruth)
  }

  test("L5 horizontal dominant svg") {
    val phoc = getPhoc(5, "h_dom.svg")

    val dTruth = Map(
      "𝑎" -> List(1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0),
      "+" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      "𝑏" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
      "𝑐" -> List(1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0),
      "𝑑" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
      "𝑒" -> List(1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0),
      "𝑓" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      "𝑔" -> List(1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1),
      "ℎ" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
    )

    assert(phoc.map == dTruth)
  }

  test("L5 horizontal-vertical dominant svg") {
    val phoc = getPhoc(5, "hv_dom.svg")

    val dTruth = Map(
      "𝑎" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
      "𝑐" -> List(1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0),
      "𝑑" -> List(1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
      "-" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
      "𝑒" -> List(1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
      "𝑓" -> List(1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1),
      "𝑔" -> List(1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1),
      "+" -> List(1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0),
      "2" -> List(1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
      "4" -> List(1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0),
      "0" -> List(1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0),
      "9" -> List(1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0),
      "𝑏" -> List(1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1),
    )

    assert(phoc.map == dTruth)
  }

  test("L5 single") {
    val phoc = getPhoc(5, "single.svg")
    println(phoc)
    val dTruth = Map(
      "𝑎" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    )

    assert(phoc.map == dTruth)
  }
}
