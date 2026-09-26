module AcuityTool.Placement where

import Prelude

import Data.Array (sortBy)
import Data.Maybe (Maybe(..))

import AcuityTool.Bed as B
import AcuityTool.Patient as P

type Placement =
  { bed     :: B.Bed
  , patient :: Maybe P.Patient
  }

patientPrio :: Maybe P.Patient -> Maybe P.Patient -> Ordering
patientPrio (Just p1) (Just p2) = P.acuityPrio p1 p2
patientPrio Nothing   (Just _)  = LT
patientPrio Nothing   _         = EQ
patientPrio _         _         = GT

sortByPrio :: Array Placement -> Array Placement
sortByPrio = sortBy $ \p q -> patientPrio p.patient q.patient
