module AcuityTool.Patient where

import Prelude

data Status = MS | IMC

derive instance eqStatus :: Eq Status

instance ordStatus :: Ord Status where
  compare IMC  MS   = GT
  compare MS   IMC  = LT
  compare _    _    = EQ

newtype Acuity = Acuity Number

derive newtype instance eqAcuity :: Eq Acuity
derive newtype instance ordAcuity :: Ord Acuity

type Patient =
  { status :: Status
  , acuity :: Acuity
  }

comparePriority :: Patient -> Patient -> Ordering
comparePriority p1 p2 = flip compare p1.acuity p2.acuity
