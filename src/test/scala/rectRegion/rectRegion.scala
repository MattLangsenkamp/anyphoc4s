package rectRegion

import org.dprl.anyphoc4s.{Compose, Tokenize, Visualize}
import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DRepr, Geo2DSpec, Geo2DTokenSet, Geo2DTokenSetSpec, Phoc, RectSpec, SVG}

import java.io.{File, PrintWriter}
import scala.io.Source

class rectRegion extends munit.FunSuite {

  val tokenizer: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] = Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet]

  def getPhoc(level: Int, svgName: String, firstBit: Boolean = true, visName: Option[String] = None): Phoc =
    val svg = SVG(Source.fromResource(svgName).mkString)
    val vertPhoc = RectSpec(endLevel = level)
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

    Compose[Geo2DTokenSet, Geo2DSpec].compose(List(vertPhoc),firstBit)(tokenSet)

  test( "L5 horizontal-vertical dominant less symbols svg first level skipped start at level 3 skip every two level" ) {
    val phoc = getPhoc( 6, firstBit = false, svgName = "pi_integral.svg")

    val dTruth: Map[String, List[Byte]] = Map(
      "𝜋"-> List(1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
    "="-> List(1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0),
    "∫"-> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
    "∞"-> List(1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0),
    "−"-> List(1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0),
    "𝑑"-> List(1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0),
    "𝑥"-> List(1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0),
    "1"-> List(1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0),
    "+"-> List(1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0),
    "2"-> List(1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
    "-"-> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    )

    assertEquals( phoc.map, dTruth )
  }

}


