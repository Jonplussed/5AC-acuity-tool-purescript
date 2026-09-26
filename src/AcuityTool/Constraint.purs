module AcuityTool.AConstraint where

import Prelude

import Data.Maybe (isNothing)
import Data.Ord (lessThanOrEq)

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl
import AcuityTool.Assignment as A
import Data.Array as Arr

type AConstraint = A.Assignment -> Pl.Placement -> Boolean

maxPatientsForIMC :: A.PlacementCount -> AConstraint
maxPatientsForIMC n a p = case A.maxStatus a p of
  Pa.IMC -> A.patientCount a p <= n
  _     -> true

maxPatientsForMS :: A.PlacementCount -> AConstraint
maxPatientsForMS n a p = case A.maxStatus a p of
  Pa.MS  -> A.patientCount a p <= n
  _     -> true

maxAcuity :: Pa.Acuity -> AConstraint
maxAcuity n a = lessThanOrEq n <<< A.totalAcuity a

distinctRooms :: AConstraint
distinctRooms a p = isNothing $ Arr.findIndex (B.isSameRoom p.bed) a.beds

-- exclusiveRooms :: AConstraint
-- exclusiveRooms a b = 
