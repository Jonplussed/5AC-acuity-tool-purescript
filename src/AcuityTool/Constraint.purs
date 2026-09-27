module AcuityTool.Constraint where

import Prim hiding (Constraint)
import Prelude

import Data.Foldable (foldl)
import Data.Maybe (isNothing)
import Data.Ord (lessThanOrEq)

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl
import AcuityTool.Assignment as A
import Data.Array as Arr

type Constraint = A.Assignment -> Pl.Placement -> Boolean

defaults :: Array Constraint
defaults =
  [ maxPatientsForIMC (A.PlacementCount 3)
  , maxPatientsForMS (A.PlacementCount 4)
  , maxAcuity (Pa.Acuity 10)
  , distinctRooms
  ]

maxPatientsForIMC :: A.PlacementCount -> Constraint
maxPatientsForIMC (A.PlacementCount n) a p =
  case A.maxStatus a p of
    Pa.IMC  -> Arr.length a.beds < n
    _       -> true

maxPatientsForMS :: A.PlacementCount -> Constraint
maxPatientsForMS (A.PlacementCount n) a p =
  case A.maxStatus a p of
    Pa.MS   -> Arr.length a.beds < n
    _       -> true

maxAcuity :: Pa.Acuity -> Constraint
maxAcuity n a = lessThanOrEq n <<< A.totalAcuity a

distinctRooms :: Constraint
distinctRooms a p = isNothing $ Arr.findIndex (B.isSameRoom p.bed) a.beds

exclusiveRooms :: Array B.Bed -> Constraint
exclusiveRooms xs a p =
  not <<< (&&) (Arr.elem p.bed xs) $
    foldl (\t b -> t || Arr.elem b xs) false a.beds
