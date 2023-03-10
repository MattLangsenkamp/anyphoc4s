package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.geo.Source
import org.dprl.anyphoc4s.model.{Spec, TokenSet}
import org.dprl.anyphoc4s.splits.Split

import scala.util.{Failure, Success, Try}
import scala.xml.Node

trait VisualizeLevel[A <: TokenSet, B <: Split, C <: Source, D <: Spec] {

  def visualize(tokenSet: A, level: List[B], spec: D): String
}

object VisualizeLevel {

  val emptySvg: Node = <svg xmlns:xlink="http://www.w3.org/1999/xlink"
                            style="fill-opacity:1; color-rendering:auto; color-interpolation:auto; text-rendering:auto; stroke:black; stroke-linecap:square; stroke-miterlimit:10; shape-rendering:auto; stroke-opacity:1; fill:black; stroke-dasharray:none; font-weight:normal; stroke-width:1; font-family:'Dialog'; font-style:normal; stroke-linejoin:miter; font-size:12px; stroke-dashoffset:0; image-rendering:auto;"
                            xmlns="http://www.w3.org/2000/svg"/>

  def apply[A <: TokenSet, B <: Split, C <: Source, D <: Spec](implicit ev: VisualizeLevel[A, B, C, D]): VisualizeLevel[A, B, C, D] = ev
}