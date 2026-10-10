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
  { placements  :: Array Placements
  -- NOTE: These are cached to reduce loops in constraint checks.
  , beds        :: Array B.Bed
  , accomCode   :: Pa.AccomCode
  , acuity      :: Pa.Acuity
  }

empty :: Assignment
empty =
  { placements: []
  , beds:       []
  , accomCode:  Pa.MS
  , acuity:     Pa.Acuity 0
  }

assign :: Assignment -> Pl.Placement -> Assignment
assign a p =
  { placements: Arr.snoc a.placements p
  , beds:       Arr.snoc a.beds p.bed
  , accomCode:  max a.accomCode p.accomCode
  , acuity:     add a.acuity p.acuity
  }

-- NOTE: Array#sortBy orders from LT to GT. Therefore, an assignment with a
-- higher "priority" to receive a patient is one that compares as LT versus
-- other assignments.
priority :: Assignment -> Assignment -> Ordering
priority a1 a2 =
  compare a1.acuity a2.acuity <>
  compare (Arr.length a1.beds) (Arr.length a2.beds) <>
  compare a1.accomCode a2.accomCode
