module Test.AcuityTool.PlacementSpec (placementSpec) where

import Prelude

import Data.Array (sortBy)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl

import Test.AcuityTool.SpecHelpers (bed, patient, place)

placementSpec :: Spec Unit
placementSpec = describe "Placement" do

  describe "patientPrio" do
    it "sorts vacant beds over filled occupied beds" do
      let p1 = Nothing
          p2 = Just $ patient Pa.MS 1

      sortBy Pl.patientPrio [p1,p2,p1] `shouldEqual` [p1,p1,p2]

    it "sorts occupied beds by acuity" do
      let p1 = Just $ patient Pa.MS   1
          p2 = Just $ patient Pa.IMC  2
          p3 = Just $ patient Pa.MS   3

      sortBy Pl.patientPrio [p2,p1,p3] `shouldEqual` [p3,p2,p1]

  describe "sortByPrio" do

    it "sorts vacant beds over filled occupied beds" do
      let p1 = place (bed 1 1) Nothing
          p2 = place (bed 1 1) (Just $ patient Pa.MS 1)

      Pl.sortByPrio [p1,p2,p1] `shouldEqual` [p1,p1,p2]

    it "sorts occupied beds by acuity" do
      let p1 = place (bed 1 1) (Just $ patient Pa.MS   1)
          p2 = place (bed 1 1) (Just $ patient Pa.IMC  2)
          p3 = place (bed 1 1) (Just $ patient Pa.MS   3)

      Pl.sortByPrio [p2,p1,p3] `shouldEqual` [p3,p2,p1]
