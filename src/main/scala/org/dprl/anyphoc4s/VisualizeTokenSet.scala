package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.model.TokenSet

trait VisualizeTokenSet[A <: TokenSet] {

  def visualize(set: A): Option[String]
}
