module Test.AcuityTool.PatientSpec (patientSpec) where

import Prelude

import Data.Array (sortBy)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.Patient as P

patientSpec :: Spec Unit
patientSpec = describe "AcuityTool.Patient" do
  describe "Status" do
    it "is orderable" do
      max P.IMC P.MS `shouldEqual` P.IMC
      min P.IMC P.MS `shouldEqual` P.MS

  describe "Patient" do
    describe "acuityPrio" do
      let p1 = { status: P.MS,   acuity: P.Acuity 1 }
          p2 = { status: P.IMC,  acuity: P.Acuity 2 }
          p3 = { status: P.MS,   acuity: P.Acuity 3 }

      it "will sort an array of patients by descending acuity" do
        sortBy P.acuityPrio [p2,p1,p3] `shouldEqual` [p3,p2,p1]

