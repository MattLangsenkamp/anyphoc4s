package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.splits.Split
import org.dprl.anyphoc4s.Geo2DRepr
import org.dprl.anyphoc4s.Geo2DRepr.LINE

sealed trait Spec {
  def numLevels: Int

  def name: String
}

trait Geo2DSpec extends Spec {
  def repr: Geo2DRepr
}

case class VertSpec(numLevels: Int, angle: Float, repr: Geo2DRepr = LINE, name: String) extends Geo2DSpec

case class HorzSpec(numLevels: Int, angle: Float, repr: Geo2DRepr = LINE, name: String) extends Geo2DSpec

case class EllipseSpec(numLevels: Int,
                       rotateDegree: Float = 0,
                       glueToHeight: Boolean = true,
                       glueToWidth: Boolean = true,
                       glueToLarger: Boolean = false,
                       scaleFactor: Float = 0,
                       repr: Geo2DRepr = LINE,
                       name: String) extends Geo2DSpec

object Spec {
  def prep[A <: TokenSet, B <: Spec, C <: Split](s: B)
                                                (using exp: Expand[B, A, C])
                                                (using ext: ExtractPhoc[A, C]): (A => PartialPhoc) = b => {
    val expanded = exp.expand(s, b)
    ext.extract(b, expanded)
  }
}