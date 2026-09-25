module AcuityTool.Patient where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Status = MS | IMC

derive instance eqStatus :: Eq Status
derive instance genericStatus :: Generic Status _

instance ordStatus :: Ord Status where
  compare IMC  MS   = GT
  compare MS   IMC  = LT
  compare _    _    = EQ

instance showStatus :: Show Status where
  show = genericShow

newtype Acuity = Acuity Int

derive newtype instance eqAcuity :: Eq Acuity
derive newtype instance ordAcuity :: Ord Acuity
derive newtype instance showAcuity :: Show Acuity

type Patient =
  { status :: Status
  , acuity :: Acuity
  }

compareAcuityDesc :: Patient -> Patient -> Ordering
compareAcuityDesc p1 p2 = flip compare p1.acuity p2.acuity
