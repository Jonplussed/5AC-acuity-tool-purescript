module AcuityTool.Roster where

import Prelude

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl
import AcuityTool.Assignment as A
import AcuityTool.Constraint as C
import Data.Array as Arr
import Data.Either as E

newtype AssignmentCount = AssignmentCount Int

derive newtype instance eqAssignmentCount :: Eq AssignmentCount
derive newtype instance ordAssignmentCount :: Ord AssignmentCount
derive newtype instance showAssignmentCount :: Show AssignmentCount

type Roster =
  { assignments :: Array A.Assignment
  , constraints :: Array C.Constraint
  }

empty :: AssignmentCount -> Array C.Constraint -> Roster
empty (AssignmentCount n) cs =
  { assignments: Arr.replicate n A.empty
  , constraints: cs
  }

-- fill :: Roster -> Array Placement -> Either String Roster
-- fill roster placements = do
--     sorted <- pure $ sortByPrio placements
--     Arr.findIndex 
