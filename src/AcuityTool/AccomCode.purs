module AcuityTool.AccomCode where

import Prelude

import Data.Bounded (class Bounded)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data AccomCode = MS | IMC

derive instance eqAccomCode :: Eq AccomCode
derive instance genericAccomCode :: Generic AccomCode _

instance ordAccomCode :: Ord AccomCode where
  compare IMC  MS   = GT
  compare MS   IMC  = LT
  compare _    _    = EQ

instance boundedAccomCode :: Bounded AccomCode where
  bottom  = MS
  top     = IMC

instance showAccomCode :: Show AccomCode where
  show = genericShow
