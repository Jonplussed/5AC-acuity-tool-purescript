module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.AcuityTool.PatientSpec (patientSpec)
import Test.AcuityTool.BedSpec (bedSpec)
import Test.AcuityTool.PlacementSpec (placementSpec)
import Test.AcuityTool.AssignmentSpec (assignmentSpec)
import Test.AcuityTool.ConstraintSpec (constraintSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  patientSpec
  bedSpec
  placementSpec
  assignmentSpec
  constraintSpec
