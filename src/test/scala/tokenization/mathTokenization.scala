package tokenization

import org.dprl.anyphoc4s.{Compose, Tokenize, Visualize}
import org.dprl.anyphoc4s.model.{Geo2DRepr, Geo2DSpec, Geo2DTokenSet, Geo2DTokenSetSpec, Phoc, RectSpec, SVG}

import java.io.{File, PrintWriter}
import scala.io.Source

class mathTokenization extends munit.FunSuite {

  val tokenizer: Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet] = Tokenize[SVG, Geo2DTokenSetSpec, Geo2DTokenSet]

  def getPhoc(level: Int, svgName: String, visName: Option[String] = None): Phoc =
    val svg = SVG( Source.fromResource( svgName ).mkString )
    val vertPhoc = RectSpec( endLevel = level )
    val tokenSet = tokenizer.convert( svg, Geo2DTokenSetSpec())

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


  test( "L3 sqrt pi svg" ) {
    val phoc = getPhoc( 4, svgName = "sqrt_pi.svg", Some("test.html")  )

    val dTruth: Map[String, List[Byte]] = Map(
      "𝜋" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      "√" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    )
    assertEquals( phoc.map, dTruth )
  }

}
