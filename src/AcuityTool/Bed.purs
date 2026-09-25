module AcuityTool.Bed where

import Prelude
import Data.Array as A

import AcuityTool.Patient as P

data Vacancy = Open | Full P.Patient

comparePriority :: Vacancy -> Vacancy -> Ordering
comparePriority (Full p1) (Full p2) = P.comparePriority p1 p2
comparePriority Open      (Full _)  = GT
comparePriority (Full _)  Open      = LT
comparePriority _         _         = EQ

newtype RoomNumber = RoomNumber Number

derive newtype instance eqRoomNumber :: Eq RoomNumber
derive newtype instance ordRoomNumber :: Ord RoomNumber

newtype BedNumber = BedNumber Number

derive newtype instance eqBedNumber :: Eq BedNumber
derive newtype instance ordBedNumber :: Ord BedNumber

type Bed =
  { roomNumber  :: RoomNumber
  , bedNumber   :: BedNumber
  , vacancy     :: Vacancy
  }

sortByPriority :: Array Bed -> Array Bed
sortByPriority = A.sortBy $ \b1 b2 -> comparePriority b1.vacancy b2.vacancy

isSameRoom :: Bed -> Bed -> Boolean
isSameRoom b1 b2 = b1.roomNumber == b2.roomNumber

isSameBed :: Bed -> Bed -> Boolean
isSameBed b1 b2 = isSameRoom b1 b2 && b1.bedNumber == b2.bedNumber
