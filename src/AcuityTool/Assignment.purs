module AcuityTool.Assignment
  ( Assignment
  , empty
  , assign
  , maxAccomCode
  , totalAcuity
  , priority
  ) where

import Prelude

import Data.Maybe (maybe)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl
import Data.Array as Arr

type Assignment =
  { beds    :: Array B.Bed
  , accom   :: Pa.AccomCode
  , acuity  :: Pa.Acuity
  }

empty :: Assignment
empty =
  { beds:   []
  , accom:  Pa.MS
  , acuity: Pa.Acuity 0
  }

assign :: Assignment -> Pl.Placement -> Assignment
assign a p =
  { beds:   Arr.snoc a.beds p.bed
  , accom:  maxAccomCode a p
  , acuity: totalAcuity a p
  }

maxAccomCode :: Assignment -> Pl.Placement -> Pa.AccomCode
maxAccomCode a p = maybe a.accom (max a.accom <<< _.accom) p.patient

totalAcuity :: Assignment -> Pl.Placement -> Pa.Acuity
totalAcuity a p = maybe a.acuity (add a.acuity <<< _.acuity) p.patient

-- Array#sortBy orders from LT to GT. Therefore, an assignment with a higher
-- "priority" to receive a patient is one that compares as LT versus other
-- assignments.
priority :: Assignment -> Assignment -> Ordering
priority a1 a2 =
  compare a1.acuity a2.acuity <>
  compare (Arr.length a1.beds) (Arr.length a2.beds) <>
  compare a1.accom a2.accom
