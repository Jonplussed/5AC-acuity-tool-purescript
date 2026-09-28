module AcuityTool.Roster where

import Prelude

import Data.Either (Either, note)
import Data.Foldable (foldl)

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl
import AcuityTool.Assignment as As
import AcuityTool.Constraint as C
import Data.Array as Ar


newtype AssignmentCount = AssignmentCount Int

derive newtype instance eqAssignmentCount :: Eq AssignmentCount
derive newtype instance ordAssignmentCount :: Ord AssignmentCount
derive newtype instance showAssignmentCount :: Show AssignmentCount

type Roster =
  { assignments :: Array As.Assignment
  , constraints :: Array C.Constraint
  }

empty :: AssignmentCount -> Array C.Constraint -> Roster
empty (AssignmentCount n) cs =
  { assignments: Ar.replicate n As.empty
  , constraints: cs
  }

fill :: Roster -> Array Pl.Placement -> Either String Roster
fill r ps = foldl (\esr -> bind esr <<< insert) (pure r) (Pl.sortByPrio ps)

insert :: Pl.Placement -> Roster -> Either String Roster
insert p r =
  note ("No valid assignment for " <> B.label p.bed) do
    i <- Ar.findIndex (\a -> Ar.all (\c -> c a p) r.constraints) r.assignments
    a <- flip As.assign p <$> Ar.index r.assignments i
    d <- Ar.deleteAt i r.assignments
    pure r { assignments = Ar.insertBy As.priority a d }

