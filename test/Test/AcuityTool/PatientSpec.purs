module Test.AcuityTool.PatientSpec (patientSpec) where

import Prelude

import Data.Array (sortBy)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.Patient as P

patientSpec :: Spec Unit
patientSpec = describe "Patient" $
  describe "compareAcuityDesc" $ do
    let p1 = { status: P.MS,   acuity: P.Acuity 1 }
    let p2 = { status: P.IMC,  acuity: P.Acuity 2 }
    let p3 = { status: P.MS,   acuity: P.Acuity 3 }

    it "will sort an array of patients by descending acuity" do
      sortBy P.compareAcuityDesc [p2,p1,p3] `shouldEqual` [p3,p2,p1]

