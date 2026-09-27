module AcuityTool.Assignment
  ( PlacementCount(..)
  , Assignment
  , empty
  , assign
  , maxStatus
  , totalAcuity
  , comparePrio
  ) where

import Prelude

import Data.Maybe (maybe)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl
import Data.Array as Arr

newtype PlacementCount = PlacementCount Int

derive newtype instance eqPlacementCount :: Eq PlacementCount
derive newtype instance ordPlacementCount :: Ord PlacementCount
derive newtype instance semiringPlacementCount :: Semiring PlacementCount
derive newtype instance showPlacementCount :: Show PlacementCount

type Assignment =
  -- { placements  :: Array Pl.Placement
  { beds    :: Array B.Bed
  , status  :: Pa.Status
  , acuity  :: Pa.Acuity
  }

empty :: Assignment
empty =
  -- { placements: []
  { beds:   []
  , status: Pa.MS
  , acuity: Pa.Acuity 0
  }

assign :: Assignment -> Pl.Placement -> Assignment
assign a p =
  -- { placements: Arr.snoc a.placements p
  { beds:   Arr.snoc a.beds p.bed
  , status: maxStatus a p
  , acuity: totalAcuity a p
  }

maxStatus :: Assignment -> Pl.Placement -> Pa.Status
maxStatus a p = maybe a.status (max a.status <<< _.status) p.patient

totalAcuity :: Assignment -> Pl.Placement -> Pa.Acuity
totalAcuity a p = maybe a.acuity (add a.acuity <<< _.acuity) p.patient

comparePrio :: Assignment -> Assignment -> Ordering
comparePrio a1 a2 =
  compare a1.acuity a2.acuity <>
  compare (Arr.length a1.beds) (Arr.length a2.beds) <>
  compare a1.status a2.status
