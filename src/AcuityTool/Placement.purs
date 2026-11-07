module AcuityTool.Placement where

import Prelude

import Data.Array (catMaybes, sortBy)
import Data.Maybe (Maybe (..), fromMaybe)

import AcuityTool.AccomCode as AC
import AcuityTool.Acuity as A
import AcuityTool.Bed as B
import AcuityTool.Patient as P

type Placement =
  { bed     :: B.Bed
  , patient :: Maybe P.Patient
  }

open :: B.Bed -> Placement
open b = { bed: b, patient: Nothing }

full :: B.Bed -> P.Patient -> Placement
full b p = { bed: b, patient: Just p }

prioritize :: Array Placement -> Array Placement
prioritize ps =
  flip sortBy ps $ \p1 p2 -> P.priority
    (fromMaybe avgPatient p1.patient)
    (fromMaybe avgPatient p2.patient)
  where
    avgPatient = P.patient AC.default $ avgAcuity ps

-- private

-- TODO: inline if this profiles as a cost center d/t multiple loops.
avgAcuity :: Array Placement -> A.Acuity
avgAcuity = A.average <<< map _.acuity <<< catMaybes <<< map _.patient
