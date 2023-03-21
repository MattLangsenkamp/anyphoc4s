package org.dprl.anyphoc4s.model

import org.dprl.anyphoc4s.model.Geo2DRepr.LINE
import org.dprl.anyphoc4s.model.{Geo2DRepr, PartialPhoc, TokenSet,Geo2DSpec, Spec}
import org.dprl.anyphoc4s.splits.Split
import org.dprl.anyphoc4s.{Expand, ExtractPhoc}

sealed trait Spec {
  
  def startLevel: Int
  def skipStep: Int
  def numLevels: Int

  def name: String
}

trait Geo2DSpec extends Spec {
  def repr: Geo2DRepr
}

case class VertSpec(startLevel: Int = 1,
                    skipStep: Int = 0,
                    numLevels: Int, 
                    angle: Float = 0, 
                    repr: Geo2DRepr = LINE, 
                    name: String) extends Geo2DSpec

case class HorzSpec(startLevel: Int = 1,
                    skipStep: Int = 0,
                    numLevels: Int, 
                    angle: Float = 0, 
                    repr: Geo2DRepr = LINE, 
                    name: String) extends Geo2DSpec

case class EllipseSpec(startLevel: Int = 1,
                       skipStep: Int = 0, 
                       numLevels: Int,
                       rotateDegree: Float = 0,
                       glueToHeight: Boolean = true,
                       glueToWidth: Boolean = true,
                       glueToLarger: Boolean = false,
                       scaleFactor: Float = 0,
                       repr: Geo2DRepr = LINE,
                       name: String) extends Geo2DSpec

case class RectSpec(startLevel: Int = 1,
                       skipStep: Int = 0,
                       numLevels: Int,
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