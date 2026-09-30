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

    it "is true if the assignment + patient acuities are <= the limit" do
      C.maxAcuity (Pa.Acuity 10) A.empty
        { acuity = Pa.Acuity 5
        } (H.place bed <<< Just $ H.patient Pa.MS 5) `shouldEqual` true

      C.maxAcuity (Pa.Acuity 10) A.empty
        { acuity = Pa.Acuity 6
        } (H.place bed <<< Just $ H.patient Pa.MS 5) `shouldEqual` false

  describe "#distinctRooms" do
    it "is true if the placement shares no rooms with assigned beds" do
      let p1 = H.place (H.bed 1 1) Nothing
          p2 = H.place (H.bed 2 1) Nothing
          a1 = A.empty { beds = [H.bed 2 2, H.bed 3 1] }

      C.distinctRooms a1 p1 `shouldEqual` true
      C.distinctRooms a1 p2 `shouldEqual` false

  describe "#exclusiveBeds" do
    let b11 = H.bed 1 1
        b22 = H.bed 2 2
        b33 = H.bed 3 3

    describe "given a placement in the list of exclusive beds" do
      let p = H.place b11 Nothing
          a = A.empty { beds = [b22] }

      it "is true if the assignment has no beds in the exclusive list" do
        C.exclusiveBeds [b11,b33] a p `shouldEqual` true

      it "is true if the placement bed is not in the exclusive list" do
        C.exclusiveBeds [b22,b33] a p `shouldEqual` true

      it "is false the both placement and assignment have beds in the list" do
        C.exclusiveBeds [b11,b22] a p `shouldEqual` false
