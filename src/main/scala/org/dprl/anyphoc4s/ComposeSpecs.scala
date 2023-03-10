package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.model.{PartialPhoc, Phoc, TokenSet}


object ComposeSpecs {
  def composeSpecs[A <: TokenSet](phocFuncList: List[A => PartialPhoc]): (A => Phoc) = (tokenSet: A) => {
    phocFuncList
      .map(_ (tokenSet))
      .reduce(
        _ append _
      ).toPhoc
  }
}
