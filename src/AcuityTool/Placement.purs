module AcuityTool.Placement where

import Prelude

import Data.Array (catMaybes, sortBy)
import Data.Maybe (Maybe (..))

import AcuityTool.AccomCode as AC
import AcuityTool.Acuity as A
import AcuityTool.Bed (Bed (..))
import AcuityTool.Patient as P
import AcuityTool.Vacancy (Vacancy (..))

type Placement_ =
  { bed     :: Bed
  , patient :: Maybe P.Patient
  }

vacant :: Bed -> Placement_
vacant b = { bed: b, patient: Nothing }

occupied :: Bed -> P.Patient -> Placement_
occupied b p = { bed: b, patient: Just p }

type Placement =
  { bed     :: Bed
  , vacancy :: Vacancy
  , patient :: P.Patient
  }

placement :: Bed -> Vacancy -> P.Patient -> Placement
placement b v p = { bed: b, vacancy: v, patient: p }

normalize :: Array Placement_ -> Array Placement
normalize ps = flip map ps $ \p ->
  case p.patient of
    Just pat -> placement p.bed Occupied pat
    Nothing  -> placement p.bed Vacant avgPatient
  where
    avgPatient = P.patient AC.default $ avgAcuity ps

prioritize :: Array Placement -> Array Placement
prioritize = sortBy \p1 p2 -> P.priority p1.patient p2.patient

-- PRIVATE

-- TODO: inline if this profiles as a cost center d/t multiple loops.
avgAcuity :: Array Placement_ -> A.Acuity
avgAcuity = A.average <<< map _.acuity <<< catMaybes <<< map _.patient
