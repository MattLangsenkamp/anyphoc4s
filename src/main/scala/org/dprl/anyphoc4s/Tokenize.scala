package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.geo.BoundingBox

trait Tokenize[A <: Source, B <: Spec, C <: TokenSet | List[CompoundTokenSet]] {
  def convert(source: A, Spec: B): C
}

object Tokenize {

  def apply[A <: Source, B <: Spec, C<: TokenSet](implicit ev: Tokenize[A, B, C]): Tokenize[A, B, C] = ev

  given svgConverter: Tokenize[SVG, HorzSpec, Geo2DTokenSet] with
    override def convert(source: SVG, Spec: HorzSpec): Geo2DTokenSet = new Geo2DTokenSet(Geo2DMeta(BoundingBox(1,1,2,2)), List(), None, None)

  /*given proConverter: Tokenize[PROTable, HorzSpec, List[ProDoc]] with
    override def convert(source: PROTable, Spec: HorzSpec): List[ProDoc] = {

      def doc2CompoundTokenSet(d: Document): ProDoc =
        ProDoc(d.filePath, d.pages.zipWithIndex.flatMap((p, i)=>p.regions.map(region2TokenSet(_, d.filePath, i))))

      def region2TokenSet(r: Region, filePath: String, page: Int): Geo2DTokenSet = {
        Geo2DTokenSet(Geo2DMeta(
          BoundingBox(
            r.boundingBox.x,
            r.boundingBox.y,
            r.boundingBox.width+r.boundingBox.x,
            r.boundingBox.height+r.boundingBox.y)),
          r.proObjects.map(o => Geo2DToken(o.classLabel ,BoundingBox(
            o.boundingBox.x,
            o.boundingBox.y,
            o.boundingBox.width+o.boundingBox.x,
            o.boundingBox.height+o.boundingBox.y))),
          Some(filePath),
          Some(page))
      }

      source.docs.foldLeft(List[ProDoc]()) ((l, d) => doc2CompoundTokenSet(d) :: l)
    }*/
}