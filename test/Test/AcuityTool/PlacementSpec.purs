module Test.AcuityTool.PlacementSpec (placementSpec) where

import Prelude

import Data.Array (sortBy)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl

placementSpec :: Spec Unit
placementSpec = describe "Placement" do
  describe "patientPrio" do
    it "sorts vacant beds over filled occupied beds" do
      let p1 = Nothing
          p2 = Just { status: Pa.MS, acuity: Pa.Acuity 1 }

      sortBy Pl.patientPrio [p1,p2,p1] `shouldEqual` [p1,p1,p2]

    it "sorts occupied beds by acuity" do
      let p1 = Just { status: Pa.MS,   acuity: Pa.Acuity 1 }
          p2 = Just { status: Pa.IMC,  acuity: Pa.Acuity 2 }
          p3 = Just { status: Pa.MS,   acuity: Pa.Acuity 3 }

      sortBy Pl.patientPrio [p2,p1,p3] `shouldEqual` [p3,p2,p1]

  describe "sortByPrio" do
    let place p = { bed: B.Bed (B.RoomNumber 1) (B.BedNumber 1), patient: p }

    it "sorts vacant beds over filled occupied beds" do
      let p1 = place Nothing
          p2 = place $ Just { status: Pa.MS, acuity: Pa.Acuity 1 }

      Pl.sortByPrio [p1,p2,p1] `shouldEqual` [p1,p1,p2]

    it "sorts occupied beds by acuity" do
      let p1 = place $ Just { status: Pa.MS,   acuity: Pa.Acuity 1 }
          p2 = place $ Just { status: Pa.IMC,  acuity: Pa.Acuity 2 }
          p3 = place $ Just { status: Pa.MS,   acuity: Pa.Acuity 3 }

      Pl.sortByPrio[p2,p1,p3] `shouldEqual` [p3,p2,p1]
