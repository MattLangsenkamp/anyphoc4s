package org.dprl.anyphoc4s.model



enum TokenUnificationPolicy {
  case PARTS
  case LEFT_ASSOCIATE
  case LEFT_ASSOCIATE_WITH_PARTS
  case FULL_WITH_PARTS
}
class TokenSetSpec(unificationPolicy: TokenUnificationPolicy)

case class Geo2DTokenSetSpec(unificationPolicy: TokenUnificationPolicy = TokenUnificationPolicy.PARTS,
                        scale: Double = 1,
                        repr: Geo2DRepr = Geo2DRepr.LINE) extends TokenSetSpec(unificationPolicy)
