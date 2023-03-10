package org.dprl.anyphoc4s.model


enum TokenUnificationPolicy {
  case PARTS
  case LEFT_ASSOCIATE
  case LEFT_ASSOCIATE_WITH_PARTS
  case FULL_WITH_PARTS
}
case class TokenSetSpec(unificationPolicy: TokenUnificationPolicy)

class Geo2DTokenSetSpec(unificationPolicy: TokenUnificationPolicy,
                   scale: Int) extends TokenSetSpec(unificationPolicy)
