package centroidRegion

import org.dprl.anyphoc4s.{Compose, Tokenize, Visualize}
import org.dprl.anyphoc4s.model.{EllipseSpec, Geo2DRepr, Geo2DSpec, Geo2DTokenSet, Geo2DTokenSetSpec, HorzSpec, Phoc, SVG, Spec, VertSpec}
import org.dprl.anyphoc4s.splits.{EllipseSplit, HorzSplit, VertSplit}

import java.io.{File, PrintWriter}
import scala.io.Source

class centroidRegion extends munit.FunSuite {


  val tokenizer: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] = Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet]

  test( "L5 horizontal dominant svg vertLine" ) {

    val svg = SVG( Source.fromResource( "h_dom.svg" ).mkString )
    val vertPhoc = VertSpec( endLevel = 5 )
    val tokenSet = tokenizer.convert( svg, Geo2DTokenSetSpec( repr = Geo2DRepr.CENTROID ) )
    val phoc = Compose[Geo2DTokenSet, Geo2DSpec].compose( List( vertPhoc ) )( tokenSet )

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "+" -> List( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ),
      "𝑏" -> List( 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "𝑐" -> List( 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0 ),
      "𝑑" -> List( 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0 ),
      "𝑒" -> List( 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0 ),
      "𝑓" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0 ),
      "𝑔" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1 ),
      "ℎ" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1 ),
    )

    assertEquals( phoc.map, dTruth )
  }

  test( "L5 vertical dominant svg vertLine" ) {

    val svg = SVG( Source.fromResource( "v_dom.svg" ).mkString )
    val vertPhoc = VertSpec( endLevel = 5 ) 
    val tokenSet = tokenizer.convert( svg, Geo2DTokenSetSpec( repr = Geo2DRepr.CENTROID ) )
    val phoc = Compose[Geo2DTokenSet, Geo2DSpec].compose( List( vertPhoc ) )( tokenSet )

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎"-> List(1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
    "𝑐"-> List(1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
    "𝑑"-> List(1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
    "-"-> List(1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0),
    "𝑒"-> List(1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
    "𝑓"-> List(1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
    "𝑔"-> List(1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
    )

    assertEquals( phoc.map, dTruth )
  }

  test( "L5 horizontal vertical dominant svg horzLine" ) {

    val svg = SVG( Source.fromResource( "hv_dom.svg" ).mkString )
    val vertPhoc = HorzSpec( endLevel = 5 ) 
    val tokenSet = tokenizer.convert( svg, Geo2DTokenSetSpec( repr = Geo2DRepr.CENTROID ) )
    val phoc = Compose[Geo2DTokenSet, Geo2DSpec].compose( List( vertPhoc ) )( tokenSet )

    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0 ),
      "𝑐" -> List( 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ),
      "𝑑" -> List( 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0 ),
      "-" -> List( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0 ),
      "𝑒" -> List( 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0 ),
      "𝑓" -> List( 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0 ),
      "𝑔" -> List( 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1 ),
      "+" -> List( 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0 ),
      "2" -> List( 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0 ),
      "4" -> List( 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0 ),
      "0" -> List( 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0 ),
      "9" -> List( 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0 ),
      "𝑏" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1 ),
    )

    assertEquals( phoc.map, dTruth )
  }

  test( "L3 horizontal vertical dominant svg ellipsisLine" ) {

    val svg = SVG( Source.fromResource( "hv_dom.svg" ).mkString )
    val vertPhoc = EllipseSpec( endLevel = 4 )
    val tokenSet = tokenizer.convert( svg, Geo2DTokenSetSpec( repr = Geo2DRepr.CENTROID ) )
    val phoc = Compose[Geo2DTokenSet, Geo2DSpec].compose( List( vertPhoc ) )( tokenSet )


    val dTruth: Map[String, List[Byte]] = Map(
      "𝑎" -> List( 1, 1, 1, 1, 1, 1, 1, 1, 1, 0 ),
      "𝑐" -> List( 1, 1, 0, 1, 0, 0, 1, 0, 0, 0 ),
      "𝑑" -> List( 1, 0, 1, 0, 1, 0, 0, 1, 0, 0 ),
      "-" -> List( 1, 1, 1, 1, 1, 0, 1, 1, 0, 0 ),
      "𝑒" -> List( 1, 0, 1, 0, 1, 0, 0, 1, 0, 0 ),
      "𝑓" -> List( 1, 1, 1, 1, 1, 0, 1, 1, 0, 0 ),
      "𝑔" -> List( 1, 1, 1, 1, 1, 0, 1, 1, 0, 0 ),
      "+" -> List( 1, 0, 1, 0, 1, 1, 0, 0, 1, 1 ),
      "2" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 1, 0 ),
      "4" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 0, 1 ),
      "0" -> List( 1, 0, 1, 0, 0, 1, 0, 0, 1, 1 ),
      "9" -> List( 1, 0, 1, 0, 1, 0, 0, 0, 1, 0 ),
      "𝑏" -> List( 1, 1, 0, 1, 0, 0, 1, 0, 0, 0 ),
    )

    assertEquals( phoc.map, dTruth )
  }
}
