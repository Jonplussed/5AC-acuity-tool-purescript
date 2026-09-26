module Test.AcuityTool.BedSpec (bedSpec) where

import Prelude

import Data.Array (sortBy)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.Bed as B
import AcuityTool.Patient as P

bedSpec :: Spec Unit
bedSpec = describe "Vacancy" $
  describe "vacancyPrio" do
    it "sorts vacant over occupied" do
      let v1 = B.Vacant
          v2 = B.Occupied { status: P.MS, acuity: P.Acuity 1 }

      sortBy B.vacancyPrio [v1,v2,v1] `shouldEqual` [v1,v1,v2]

    it "sorts occupied by acuity" do
      let v1 = B.Occupied { status: P.MS,   acuity: P.Acuity 1 }
          v2 = B.Occupied { status: P.IMC,  acuity: P.Acuity 2 }
          v3 = B.Occupied { status: P.MS,   acuity: P.Acuity 3 }

      sortBy B.vacancyPrio [v2,v1,v3] `shouldEqual` [v3,v2,v1]
