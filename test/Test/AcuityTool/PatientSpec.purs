module Test.AcuityTool.PatientSpec (patientSpec) where

import Prelude

import Data.Array (sortBy)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.AccomCode (AccomCode (..))
import AcuityTool.Patient as P

patientSpec :: Spec Unit
patientSpec = describe "AcuityTool.Patient" do

  describe "priority" do
    let p1 = P.patient MS  1
        p2 = P.patient IMC 1
        p3 = P.patient MS  2

    it "orders patients by descending acuity then descending accomCode" do
      sortBy P.priority [p1,p2,p3] `shouldEqual` [p3,p2,p1]

