module AcuityTool.Assignment
  ( Assignment
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

type Assignment =
  { beds    :: Array B.Bed
  , status  :: Pa.Status
  , acuity  :: Pa.Acuity
  }

empty :: Assignment
empty =
  { beds:   []
  , status: Pa.MS
  , acuity: Pa.Acuity 0
  }

assign :: Assignment -> Pl.Placement -> Assignment
assign a p =
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
