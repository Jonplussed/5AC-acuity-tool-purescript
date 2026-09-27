module Test.AcuityTool.AssignmentSpec (assignmentSpec) where

import Prelude

import Data.Array (sortBy)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl
import AcuityTool.Assignment as A

assignmentSpec :: Spec Unit
assignmentSpec = describe "Assignment" do
  describe "assign" do
    let a = A.empty

    pending "adds the beds to the beds array"

    pending "total the acuity of all patients"

    pending "retains the maximum status of all patients"

