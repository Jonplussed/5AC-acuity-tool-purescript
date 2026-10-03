module AcuityTool.Patient where

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

newtype Acuity = Acuity Int

derive newtype instance eqAcuity :: Eq Acuity
derive newtype instance ordAcuity :: Ord Acuity
derive newtype instance boundedAcuity :: Bounded Acuity
derive newtype instance semiringAcuity :: Semiring Acuity
derive newtype instance showAcuity :: Show Acuity

data Patient = Patient AccomCode Acuity

derive instance eqPatient :: Eq Patient
derive instance genericPatient :: Generic Patient

instance ordPatient :: Ord Patient where
  compare (Patient accom1 acuity1) (Patient accom2 acuity2) =
    compare acuity1 acuity2 <> compare accom1 accom2

instance showPatient :: Show Patient where
  show = genericShow
