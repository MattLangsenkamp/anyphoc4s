package representationScaling

import org.dprl.anyphoc4s.{Compose, Tokenize, Visualize}
import org.dprl.anyphoc4s.model.{Geo2DRepr, Geo2DSpec, Geo2DTokenSet, Geo2DTokenSetSpec, Phoc, RectSpec, SVG, VertSpec}

import java.io.{File, PrintWriter}
import scala.io.Source

class representationScaling extends munit.FunSuite {

  val tokenizer: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] = Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet]

  def getPhoc(level: Int, svgName: String, scale: Double, repr: Geo2DRepr, visName: Option[String] = None): Phoc =
    val svg = SVG( Source.fromResource( svgName ).mkString )
    val vertPhoc = RectSpec( endLevel = level )
    val tokenSet = tokenizer.convert( svg, Geo2DTokenSetSpec( repr = repr, scale = scale ) )

    visName.foreach { s =>
      val str = Visualize[Geo2DTokenSet, Geo2DSpec, SVG].visualize(
        tokenSet,
        List( RectSpec( endLevel = level ) ),
        svg
      )
      val writer = new PrintWriter( new File( s ) )
      writer.print( str )
      writer.close()
    }

    Compose[Geo2DTokenSet, Geo2DSpec].compose( List( vertPhoc ) )( tokenSet )

  // .75 bbox
  test( "L3 hv dominant less symbols svg scaled down bbox" ) {
    val phoc = getPhoc( 4, scale = .75, repr = Geo2DRepr.BBOX, svgName = "hv_dom_less_sym.svg" )
    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 1, 1, 1, 0, 1, 1, 1, 0 ),
      "𝑐" -> List( 1, 1, 0, 1, 0, 0, 1, 0, 0, 0 ),
      "-" -> List( 1, 1, 1, 1, 0, 1, 1, 1, 0, 1 ),
      "+" -> List( 1, 1, 1, 0, 1, 0, 0, 1, 1, 0 ),
      "𝑏" -> List( 1, 1, 1, 1, 1, 1, 1, 1, 1, 0 ),
      "−" -> List( 1, 0, 1, 0, 1, 1, 0, 0, 1, 0 )
    )
    assertEquals( phoc.map, dTruth )
  }

  test( "L3 single svg scaled down line" ) {
    val phoc = getPhoc( 4, scale = .70, repr = Geo2DRepr.LINE, svgName = "single.svg" )
    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 1, 1, 1, 1, 0, 1, 1, 1 ),
    )
    assertEquals( phoc.map, dTruth )
  }

  test( "L3 hv dominant less symbols svg scaled down line" ) {
    val phoc = getPhoc( 4, scale = 0.75, repr = Geo2DRepr.LINE, svgName = "hv_dom_less_sym.svg")
    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 0, 1, 1, 0, 1, 1, 0, 0 ),
      "𝑐" -> List( 1, 1, 0, 1, 0, 0, 1, 0, 0, 0 ),
      "-" -> List( 1, 1, 1, 1, 0, 1, 1, 1, 0, 1 ),
      "+" -> List( 1, 1, 1, 0, 1, 0, 0, 1, 1, 0 ),
      "𝑏" -> List( 1, 1, 1, 1, 1, 0, 1, 0, 1, 0 ),
      "−" -> List( 1, 0, 1, 0, 1, 1, 0, 0, 1, 0 )
    )
    assertEquals( phoc.map, dTruth )
  }

  test( "L3 hv dominant less symbols svg scaled up bbox" ) {
    val phoc = getPhoc( 3, scale = 2, repr = Geo2DRepr.BBOX, svgName = "hv_dom_less_sym.svg" )
    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 1, 1, 1, 1 ),
      "𝑐" -> List( 1, 1, 0, 1, 0, 0 ),
      "-" -> List( 1, 1, 1, 1, 1, 1 ),
      "+" -> List( 1, 1, 1, 1, 1, 1 ),
      "𝑏" -> List( 1, 1, 1, 1, 1, 1 ),
      "−" -> List( 1, 1, 1, 0, 1, 1 )
    )
    assertEquals( phoc.map, dTruth )
  }

  test( "L3 hv dominant less symbols svg scaled up line" ) {
    val phoc = getPhoc( 3, scale = 2, repr = Geo2DRepr.LINE, svgName = "hv_dom_less_sym.svg" )
    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 0, 1, 1, 0 ),
      "𝑐" -> List( 1, 1, 0, 1, 0, 0 ),
      "-" -> List( 1, 1, 1, 1, 1, 1 ),
      "+" -> List( 1, 1, 1, 1, 1, 1 ),
      "𝑏" -> List( 1, 1, 1, 1, 1, 0 ),
      "−" -> List( 1, 1, 1, 0, 1, 1 )
    )
    assertEquals( phoc.map, dTruth )
  }
}
