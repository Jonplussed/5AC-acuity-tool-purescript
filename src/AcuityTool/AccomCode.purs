module AcuityTool.AccomCode where

import Prelude

import Data.Bounded (class Bounded)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data AccomCode = OBS | DP | MS | IMC

derive instance eqAccomCode :: Eq AccomCode
derive instance genericAccomCode :: Generic AccomCode _
derive instance ordAccomCode :: Ord AccomCode

instance boundedAccomCode :: Bounded AccomCode where
  bottom  = OBS
  top     = IMC

instance semigroupAccomCode :: Semigroup AccomCode where
  append = max

instance showAccomCode :: Show AccomCode where
  show = genericShow

