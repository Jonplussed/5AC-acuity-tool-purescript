module Test.AcuityTool.BedSpec (bedSpec) where

import Prelude

import Data.Array (sortBy)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.Bed as B
import AcuityTool.Patient as P

bedSpec :: Spec Unit
bedSpec = describe "Bed" do
  describe "patientPrio" do
    it "sorts empty beds over filled" do
      let p1 = Nothing
          p2 = Just { status: P.MS, acuity: P.Acuity 1 }

      sortBy B.patientPrio [p1,p2,p1] `shouldEqual` [p1,p1,p2]

    it "sorts occupied by acuity" do
      let p1 = Just { status: P.MS,   acuity: P.Acuity 1 }
          p2 = Just { status: P.IMC,  acuity: P.Acuity 2 }
          p3 = Just { status: P.MS,   acuity: P.Acuity 3 }

      sortBy B.patientPrio [p2,p1,p3] `shouldEqual` [p3,p2,p1]

  pending "isSameRoom"
  pending "isSameBed"
