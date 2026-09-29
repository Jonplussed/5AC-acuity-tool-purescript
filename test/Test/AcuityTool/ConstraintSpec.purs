module Test.AcuityTool.ConstraintSpec (constraintSpec) where

import Prelude

import Data.Array (replicate, sortBy)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl
import AcuityTool.Constraint as C
import AcuityTool.Assignment as A

import Test.AcuityTool.SpecHelpers as H

constraintSpec :: Spec Unit
constraintSpec = describe "Constraint" do
  describe "#maxPatientsForIMC" do
    let bed = H.bed 1 1

    it "is always true if the accomodation is MS" do
      C.maxPatientsForIMC (C.BedCount 3) A.empty
        { beds = replicate 100 bed
        , accom = Pa.MS
        } (H.place bed Nothing) `shouldEqual` true

    it "is true if IMC and with room for another bed" do
      C.maxPatientsForIMC (C.BedCount 3) A.empty
        { beds = replicate 2 bed
        , accom = Pa.IMC
        } (H.place bed Nothing) `shouldEqual` true

    it "is false if IMC and at or above the bed threshold" do
      C.maxPatientsForIMC (C.BedCount 3) A.empty
        { beds = replicate 3 bed
        , accom = Pa.IMC
        } (H.place bed Nothing) `shouldEqual` false

  describe "#maxPatientsForMS" do
    let bed = H.bed 1 1

    it "is always true if the accomodation is IMC" do
      C.maxPatientsForMS (C.BedCount 3) A.empty
        { beds = replicate 100 bed
        , accom = Pa.IMC
        } (H.place bed Nothing) `shouldEqual` true

    it "is true if MS and with room for another bed" do
      C.maxPatientsForMS (C.BedCount 3) A.empty
        { beds = replicate 2 bed
        , accom = Pa.MS
        } (H.place bed Nothing) `shouldEqual` true

    it "is false if MS and at or above the bed threshold" do
      C.maxPatientsForMS (C.BedCount 3) A.empty
        { beds = replicate 3 bed
        , accom = Pa.MS
        } (H.place bed Nothing) `shouldEqual` false

  describe "#maxAcuity" do
    let bed = H.bed 1 1

    it "is true if the assignment + patient acuities are below the limit" do
      C.maxAcuity (Pa.Acuity 10) A.empty
        { acuity = Pa.Acuity 4
        } (H.place bed <<< Just $ H.patient Pa.MS 5) `shouldEqual` true

    it "is true if the assignment + patient acuities are below the limit" do
      C.maxAcuity (Pa.Acuity 10) A.empty
        { acuity = Pa.Acuity 6
        } (H.place bed <<< Just $ H.patient Pa.MS 5) `shouldEqual` false
