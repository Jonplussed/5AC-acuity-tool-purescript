module Test.AcuityTool.BedSpec (bedSpec) where

import Prelude

import Data.Array (sortBy)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)

import AcuityTool.Bed as B

bedSpec :: Spec Unit
bedSpec = describe "Bed" do
  describe "instance Eq Bed" do
    pending "is equal if the room # and bed # are equal"

  describe "instance Ord Bed" do
    pending "is ordered by room # then bed #"

  describe "isSameRoom" do
    pending "is true if the bed #s are equal"
