module Test.AcuityTool.AccomCodeSpec (accomCodeSpec) where

import Prelude

import Data.Array (sort)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.AccomCode (AccomCode (..))

accomCodeSpec :: Spec Unit
accomCodeSpec = describe "AcuityTool.AccomCode" do

  describe "instance Ord AccomCode" do
    it "is sortable" do
      sort [MS, IMC, MS] `shouldEqual` [MS, MS, IMC]
