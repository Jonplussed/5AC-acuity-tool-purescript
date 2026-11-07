module Test.AcuityTool.AcuitySpec (acuitySpec) where

import Prelude

import Data.Array (sort)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.Acuity (Acuity (..))
import AcuityTool.Acuity as A

acuitySpec :: Spec Unit
acuitySpec = describe "AcuityTool.AcuitySpec" do

  describe "average" do
    it "is the average integer acuity with the decimal rounded" do
      A.average [Acuity 2, Acuity 2, Acuity 3] `shouldEqual` Acuity 2
      A.average [Acuity 2, Acuity 3, Acuity 3] `shouldEqual` Acuity 3

    it "is zero when the array of acuities is empty" do
      A.average [] `shouldEqual` Acuity 0
