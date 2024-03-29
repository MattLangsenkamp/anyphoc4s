package org.dprl.anyphoc4s

import org.dprl.anyphoc4s.model.{Geo2DToken, Token}
import org.dprl.anyphoc4s.splits.{Geo2DSplit, Split}

trait TestMembership[A <: Split, B <: Token] {

  def test(split: A, token: B): Byte

}

object TestMembership {

  def apply[A <: Split, B <: Token](implicit ev: TestMembership[A, B]): TestMembership[A, B] = ev

  given geo2DTester: TestMembership[Geo2DSplit, Geo2DToken] with
    override def test(split: Geo2DSplit, token: Geo2DToken): Byte =
      if (split.phocRegion.contains(token.boundingBox.poly)) {
        1
      } else if (split.phocRegion.intersects(token.repr(token.repr2d))) {
        1
      } else {
        0
      }
}
