module AcuityTool.Placement
  ( Placement
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

-- TODO:  Account for blocked beds?
data Vacancy = Vacant | Occupied

type PlaceFormData =
  { bed     :: B.Bed
  , patient :: Maybe P.Patient
  }

type Placement =
  { bed     :: B.Bed
  , vacancy :: Vacancy
  , patient :: P.Patient
  }

-- NOTE: Cache the avg calculation in an argument for efficiency.
normalize :: Array PlaceFormData -> Array Placement
normalize pfds = foldl (fn $ avgAcuity pfds) [] pfds
  where
    fn avg pls pfd =
      snoc pls $ case pfd.patient of
        Just pat ->
          { bed:      pfd.bed
          , vacancy:  Occupied
          , patient:  pat
          }
        Nothing ->
          { bed:      pfd.bed
          , vacancy:  Vacant
          , patient:  P.Patient bottom avg
          }

-- TODO: should we consider the occupancy when determining priority?
sortByPrio :: Array Placement -> Array Placement
sortByPrio = sortBy $ \p q -> compare q.patient p.patient

-- private

avgAcuity :: Array PlaceFormData -> P.Acuity
avgAcuity pls = P.Acuity $ a / c
  where
    (Tuple (P.Acuity a) c) =
      foldl totalAcuity (Tuple (P.Acuity 0) 0) pls

totalAcuity :: Tuple P.Acuity Int -> PlaceFormData -> Tuple P.Acuity Int
totalAcuity t@(Tuple a n) pl =
  maybe t (\pa -> Tuple (a + pa.acuity) (n + 1)) pl.patient

