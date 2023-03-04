package org.dprl.anyphoc4s

object ComposeSpecs {
  def composeSpecs[A <: TokenSet](phocFuncList: List[A => PartialPhoc]): (A => Phoc) = (tokenSet: A) => {
    phocFuncList
      .map(_ (tokenSet))
      .reduce(
        _ append _
      ).toPhoc
  }
}
