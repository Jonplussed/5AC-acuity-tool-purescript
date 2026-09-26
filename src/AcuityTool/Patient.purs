module AcuityTool.Patient where

import Prelude

import Data.Bounded (class Bounded)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Status = MS | IMC

derive instance eqStatus :: Eq Status
derive instance genericStatus :: Generic Status _

instance ordStatus :: Ord Status where
  compare IMC  MS   = GT
  compare MS   IMC  = LT
  compare _    _    = EQ

instance boundedStatus :: Bounded Status where
  bottom  = MS
  top     = IMC

instance showStatus :: Show Status where
  show = genericShow

newtype Acuity = Acuity Int

derive newtype instance eqAcuity :: Eq Acuity
derive newtype instance ordAcuity :: Ord Acuity
derive newtype instance boundedAcuity :: Bounded Acuity
derive newtype instance semiringAcuity :: Semiring Acuity
derive newtype instance showAcuity :: Show Acuity

type Patient =
  { status :: Status
  , acuity :: Acuity
  }

acuityPrio :: Patient -> Patient -> Ordering
acuityPrio p1 p2 = compare p2.acuity p1.acuity
