module AcuityTool.Bed where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import AcuityTool.Patient as P

newtype RoomNumber = RoomNumber Int

derive newtype instance eqRoomNumber :: Eq RoomNumber
derive newtype instance ordRoomNumber :: Ord RoomNumber
derive newtype instance showRoomNumber :: Show RoomNumber

newtype BedNumber = BedNumber Int

derive newtype instance eqBedNumber :: Eq BedNumber
derive newtype instance ordBedNumber :: Ord BedNumber
derive newtype instance showBedNumber :: Show BedNumber

data Bed = Bed RoomNumber BedNumber

derive instance genericBed :: Generic Bed _

instance eqBed :: Eq Bed where
  eq (Bed r1 b1) (Bed r2 b2) = r1 == r2 && b1 == b2

instance ordBed :: Ord Bed where
  compare (Bed r1 b1) (Bed r2 b2) = compare r1 r2 <> compare b1 b2

instance showBed :: Show Bed where
  show = genericShow

isSameRoom :: Bed -> Bed -> Boolean
isSameRoom (Bed r1 _) (Bed r2 _) = r1 == r2
