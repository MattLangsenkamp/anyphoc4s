package org.dprl.anyphoc4s

trait VisualizeTokenSet[A <: TokenSet] {

  def visualize(set: A): Option[String]
}
