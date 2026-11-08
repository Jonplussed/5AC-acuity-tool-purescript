module Test.AcuityTool.PlacementSpec where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.AccomCode (AccomCode (..))
import AcuityTool.Acuity (Acuity (..))
import AcuityTool.Bed (Bed (..))
import AcuityTool.Patient (patient)
import AcuityTool.Placement as P

placementSpec :: Spec Unit
placementSpec = describe "AcuityTool.Placement" do

  describe "prioritize" do
    it "sorts placements by patient priority" do
      let p1 = P.full (Bed 1 1) (patient MS  $ Acuity 1)
          p2 = P.full (Bed 1 2) (patient IMC $ Acuity 1)
          p3 = P.full (Bed 2 1) (patient MS  $ Acuity 2)

      P.prioritize [p1,p2,p3] `shouldEqual` [p3,p2,p1]

    it "sorts open placements as if they were the average of existing patients" do
      let p1 = P.open (Bed 1 1)
          p2 = P.full (Bed 1 2) (patient IMC $ Acuity 1)
          p3 = P.full (Bed 2 1) (patient MS  $ Acuity 3)

      P.prioritize [p1,p2,p3] `shouldEqual` [p3,p1,p2]
