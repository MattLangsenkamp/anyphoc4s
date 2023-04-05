package levelSkipping

import org.dprl.anyphoc4s.{Compose, Tokenize, Visualize}
import org.dprl.anyphoc4s.model.{Geo2DRepr, Geo2DSpec, Geo2DTokenSet, Geo2DTokenSetSpec, Phoc, SVG, Spec, VertSpec}
import org.dprl.anyphoc4s.splits.VertSplit

import java.io.{File, PrintWriter}
import scala.io.Source

class levelSkipping extends munit.FunSuite {

  val tokenizer: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] = Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet]

  def getPhoc(level: Int, svgName: String, firstBit: Boolean, startLevel: Int = 1, skipStep: Int = 0, visName: Option[String] = None): Phoc =
    val svg = SVG( Source.fromResource( svgName ).mkString )
    val vertPhoc = VertSpec( endLevel = level, startLevel = startLevel, skipStep = skipStep, name = f"vertPhocStart-$startLevel" )
    val tokenSet = tokenizer.convert( svg, Geo2DTokenSetSpec( repr = Geo2DRepr.BBOX ) )

    visName.foreach { s =>
      val str = Visualize[Geo2DTokenSet, Geo2DSpec, SVG].visualize(
        tokenSet,
        List( VertSpec( endLevel = level, skipStep = skipStep, startLevel = startLevel ) ),
        svg,
        firstBit
      )
      val writer = new PrintWriter( new File( s ) )
      writer.print( str )
      writer.close()
    }

    Compose[Geo2DTokenSet, Geo2DSpec].compose( List( vertPhoc ), firstBit )( tokenSet )


  test( "L5 horizontal-vertical dominant svg first level skipped" ) {
    val phoc = getPhoc( 5, svgName = "hv_dom.svg", firstBit = false )

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1 ),
      "𝑐" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "𝑑" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "-" -> List( 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1 ),
      "𝑒" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "𝑓" -> List( 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1 ),
      "𝑔" -> List( 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1 ),
      "+" -> List( 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0 ),
      "2" -> List( 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0 ),
      "4" -> List( 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0 ),
      "0" -> List( 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0 ),
      "9" -> List( 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0 ),
      "𝑏" -> List( 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1 ),
    )

    assertEquals( phoc.map, dTruth )
  }

  test( "L5 horizontal-vertical dominant svg first level skipped start at level 2" ) {
    val phoc = getPhoc( 5, svgName = "hv_dom.svg", firstBit = false, startLevel = 2 )

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1 ),
      "𝑐" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "𝑑" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "-" -> List( 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1 ),
      "𝑒" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "𝑓" -> List( 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1 ),
      "𝑔" -> List( 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1 ),
      "+" -> List( 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0 ),
      "2" -> List( 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0 ),
      "4" -> List( 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0 ),
      "0" -> List( 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0 ),
      "9" -> List( 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0 ),
      "𝑏" -> List( 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1 ),
    )

    assertEquals( phoc.map, dTruth )
  }

  test( "L5 horizontal-vertical dominant svg first level skipped start at level 3" ) {
    val phoc = getPhoc( 5, svgName = "hv_dom.svg", firstBit = false, startLevel = 3 )

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1 ),
      "𝑐" -> List( 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "𝑑" -> List( 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "-" -> List( 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1 ),
      "𝑒" -> List( 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "𝑓" -> List( 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1 ),
      "𝑔" -> List( 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1 ),
      "+" -> List( 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0 ),
      "2" -> List( 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0 ),
      "4" -> List( 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0 ),
      "0" -> List( 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0 ),
      "9" -> List( 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0 ),
      "𝑏" -> List( 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1 ),
    )

    assertEquals( phoc.map, dTruth )
  }

  test( "L5 horizontal-vertical dominant svg first level skipped start at level 5" ) {
    val phoc = getPhoc( 5, svgName = "hv_dom.svg", firstBit = false, startLevel = 5 )

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 0, 0, 1 ),
      "𝑐" -> List( 1, 0, 0, 0, 0 ),
      "𝑑" -> List( 1, 0, 0, 0, 0 ),
      "-" -> List( 1, 0, 0, 0, 1 ),
      "𝑒" -> List( 1, 0, 0, 0, 0 ),
      "𝑓" -> List( 1, 0, 0, 0, 1 ),
      "𝑔" -> List( 1, 0, 0, 0, 1 ),
      "+" -> List( 1, 1, 1, 0, 0 ),
      "2" -> List( 0, 1, 0, 0, 0 ),
      "4" -> List( 0, 0, 1, 0, 0 ),
      "0" -> List( 0, 0, 1, 1, 0 ),
      "9" -> List( 0, 0, 0, 1, 0 ),
      "𝑏" -> List( 0, 0, 0, 0, 1 ),
    )

    assertEquals( phoc.map, dTruth )
  }

  test( "L5 horizontal-vertical dominant less symbols svg first level skipped start at level 3 skip every two level" ) {
    val phoc = getPhoc( 9, svgName = "hv_dom_less_sym.svg", firstBit = false, startLevel = 3, skipStep = 2, Some("testt.html") )

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0 ),
      "𝑐" -> List( 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1 ),
      "-" -> List( 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1 ),
      "+" -> List( 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0 ),
      "𝑏" -> List( 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "−" -> List( 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0 )
    )

    assertEquals( phoc.map, dTruth )
  }

  test( "L5 horizontal-vertical dominant svg keep first level skip even" ) {
    val phoc = getPhoc( 5, svgName = "hv_dom.svg", firstBit = true, startLevel = 3, skipStep = 1 )

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 1, 1, 1, 1, 0, 0, 1 ),
      "𝑐" -> List( 1, 1, 0, 0, 1, 0, 0, 0, 0 ),
      "𝑑" -> List( 1, 1, 0, 0, 1, 0, 0, 0, 0 ),
      "-" -> List( 1, 1, 0, 1, 1, 0, 0, 0, 1 ),
      "𝑒" -> List( 1, 1, 0, 0, 1, 0, 0, 0, 0 ),
      "𝑓" -> List( 1, 1, 0, 1, 1, 0, 0, 0, 1 ),
      "𝑔" -> List( 1, 1, 0, 1, 1, 0, 0, 0, 1 ),
      "+" -> List( 1, 1, 1, 0, 1, 1, 1, 0, 0 ),
      "2" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 0 ),
      "4" -> List( 1, 0, 1, 0, 0, 0, 1, 0, 0 ),
      "0" -> List( 1, 0, 1, 1, 0, 0, 1, 1, 0 ),
      "9" -> List( 1, 0, 0, 1, 0, 0, 0, 1, 0 ),
      "𝑏" -> List( 1, 0, 0, 1, 0, 0, 0, 0, 1 ),
    )


    assertEquals( phoc.map, dTruth )
  }

}
