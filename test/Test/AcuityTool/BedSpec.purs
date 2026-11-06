module Test.AcuityTool.BedSpec (bedSpec) where

import Prelude

import Data.Array (sort)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

import AcuityTool.Bed (Bed (..))
import AcuityTool.Bed as B

bedSpec :: Spec Unit
bedSpec = describe "AcuityTool.Bed" do

  describe "instance Eq Bed" do
    it "is equal if the room # and bed # are equal" do
      Bed 2 1 `shouldEqual` Bed 2 1
      Bed 2 2 `shouldNotEqual` Bed 2 1
      Bed 3 1 `shouldNotEqual` Bed 2 1

  describe "instance Ord Bed" do
    let bed11 = Bed 1 1
        bed13 = Bed 1 3
        bed22 = Bed 2 2
        bed31 = Bed 3 1

    it "is ordered by room # then bed #" do
      sort [bed31,bed13,bed22,bed11] `shouldEqual` [bed11,bed13,bed22,bed31]

  describe "isSameRoom" do
    it "is true if the room #s are equal" do
      B.isSameRoom (Bed 2 1) (Bed 2 3) `shouldEqual` true
      B.isSameRoom (Bed 2 1) (Bed 3 1) `shouldEqual` false

  describe "label" do
    it "is the string of the room and bed #s" do
      B.label (Bed 3 4) `shouldEqual` "3-4"
