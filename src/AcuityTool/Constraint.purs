module AcuityTool.AConstraint where

import Prelude

import Data.Maybe (isNothing)
import Data.Ord (lessThanOrEq)

import Data.Array as Arr
import AcuityTool.Patient as P
import AcuityTool.Bed as B
import AcuityTool.Assignment as A

type AConstraint = A.Assignment -> B.Bed -> Boolean

maxPatientsForIMC :: A.PatientCount -> AConstraint
maxPatientsForIMC n a b = case A.maxStatus a b of
  P.IMC -> A.patientCount a b <= n
  _     -> true

maxPatientsForMS :: A.PatientCount -> AConstraint
maxPatientsForMS n a b = case A.maxStatus a b of
  P.MS  -> A.patientCount a b <= n
  _     -> true

maxAcuity :: P.Acuity -> AConstraint
maxAcuity n a = lessThanOrEq n <<< A.totalAcuity a

distinctRooms :: AConstraint
distinctRooms a b = isNothing $ Arr.findIndex (B.isSameRoom b) a.beds

-- exclusiveRooms :: AConstraint
-- exclusiveRooms a b = 
