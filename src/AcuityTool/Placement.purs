module AcuityTool.Placement
  ( VacancyUI (..)
  , PlacementUI (..)
  , Vacancy (..)
  , Placement (..)
  , normalize
  , sortByPrio
  ) where

import Prelude

import Data.Array (foldl, snoc, sortBy)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..), fst)

import AcuityTool.Bed as B
import AcuityTool.Patient as P

-- NOTE: The user input differs slightly from the assignment data model and we
-- (possibly) want to present the user with both on submission.
data VacancyUI = VacantUI | OccupiedUI P.Patient | BlockedUI

type PlacementUI =
  { bed     :: B.Bed
  , patient :: VacancyUI
  }

data Vacancy = Vacant | Occupied

type Placement =
  { bed     :: B.Bed
  , vacancy :: Vacancy
  , patient :: P.Patient
  }

-- NOTE: Cache the avg calculation in an argument for efficiency.
normalize :: Array PlacementUI -> Array Placement
normalize puis = foldl (fn $ avgAcuity puis) [] puis
  where
    fn avg pls pui = case pui.patient of
        -- NOTE: For assignment purposes, an open bed is treated as a patient
        -- with lowest acuity and average (of submitted) acuities.
        VacantUI -> snoc pls
          { bed:      pui.bed
          , vacancy:  Vacant
          , patient:  P.Patient bottom avg
          }
        OccupiedUI p -> snoc pls
          { bed:      pui.bed
          , vacancy:  Occupied
          , patient:  p
          }
        -- NOTE: Blocked beds are eliminated from the assignment generation.
        BlockedUI -> pls

sortByPrio :: Array Placement -> Array Placement
sortByPrio = sortBy $ \p q -> compare q.patient p.patient

-- private

avgAcuity :: Array PlacementUI -> P.Acuity
avgAcuity pls = P.Acuity $ a / c -- TODO: What is the rounding method here?
  where
    (Tuple (P.Acuity a) c) =
      foldl totalAcuity (Tuple (P.Acuity 0) 0) pls

totalAcuity :: Tuple P.Acuity Int -> PlacementUI -> Tuple P.Acuity Int
totalAcuity t@(Tuple a n) pui =
  case pui.patient of
    VacantUI -> t
    OccupiedUI p -> Tuple (a + P.acuity p) (n + 1)
    BlockedUI -> t

