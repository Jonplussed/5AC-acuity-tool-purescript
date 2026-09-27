module Test.AcuityTool.BedSpec (bedSpec) where

import Prelude

import Data.Array (sort)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

import AcuityTool.Bed as B

import Test.AcuityTool.SpecHelpers (bed)

bedSpec :: Spec Unit
bedSpec = describe "Bed" do

  describe "instance Eq Bed" do
    it "is equal if the room # and bed # are equal" do
      bed 2 1 `shouldEqual` bed 2 1
      bed 2 2 `shouldNotEqual` bed 2 1
      bed 3 1 `shouldNotEqual` bed 2 1

  describe "instance Ord Bed" do
    let bed11 = bed 1 1
        bed13 = bed 1 3
        bed22 = bed 2 2
        bed31 = bed 3 1

    it "is ordered by room # then bed #" do
      sort [bed31,bed13,bed22,bed11] `shouldEqual` [bed11,bed13,bed22,bed31]

  describe "isSameRoom" do
    it "is true if the bed #s are equal" do
      B.isSameRoom (bed 2 1) (bed 2 3) `shouldEqual` true
      B.isSameRoom (bed 2 1) (bed 3 1) `shouldEqual` false
