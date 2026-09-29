module Test.AcuityTool.AssignmentSpec (assignmentSpec) where

import Prelude

import Data.Array (sortBy)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl
import AcuityTool.Assignment as A

import Test.AcuityTool.SpecHelpers (bed, patient, place)

assignmentSpec :: Spec Unit
assignmentSpec = describe "Assignment" do
  describe "#assign" do
    let a = A.empty

    a <- pure <<< A.assign a $ place (bed 1 2) (Just $ patient Pa.IMC 1)
    a <- pure <<< A.assign a $ place (bed 3 1) Nothing
    a <- pure <<< A.assign a $ place (bed 2 1) (Just $ patient Pa.MS 3)

    it "adds the beds to the beds array" do
      a.beds `shouldEqual` [bed 1 2, bed 3 1, bed 2 1]

    it "totals the acuity of all patients" do
      a.acuity `shouldEqual` Pa.Acuity 4

    it "keeps the maximum accomodation code of all patients" do
      a.accom `shouldEqual` Pa.IMC

  describe "#priority" do
    it "prioritizes lower acuity over all else" do
      let a1 = A.assign A.empty $ place (bed 1 1) (Just $ patient Pa.MS 5)
          a2 = A.assign A.empty $ place (bed 1 1) Nothing
          a3 = A.assign A.empty $ place (bed 1 1) (Just $ patient Pa.IMC 2)

      sortBy A.priority [a1,a2,a3] `shouldEqual` [a2,a3,a1]

    describe "given the same acuity" do
      it "prioritizes few beds over more beds" do
        let a1 = A.assign A.empty $ place (bed 1 1) (Just $ patient Pa.MS 3)
            a2 = A.assign A.empty $ place (bed 1 1) (Just $ patient Pa.MS 3)

        a1 <- pure <<< A.assign a1 $ place (bed 1 1) Nothing
        compare a1 a2 `shouldEqual` GT

    describe "given the same acuity and # of beds" do
      it "prioritizies MS accomodation over IMC" do
        let a1 = A.assign A.empty $ place (bed 1 1) (Just $ patient Pa.MS 3)
            a2 = A.assign A.empty $ place (bed 1 1) (Just $ patient Pa.IMC 3)

        compare a1 a2 `shouldEqual` LT




