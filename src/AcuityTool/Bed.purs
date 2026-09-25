module AcuityTool.Bed where

import Prelude
import Data.Array (sortBy)

import AcuityTool.Patient as P

data Vacancy = Open | Full P.Patient

compareAssignPrio :: Vacancy -> Vacancy -> Ordering
compareAssignPrio (Full p1) (Full p2) = P.compareAcuityDesc p1 p2
compareAssignPrio Open      (Full _)  = GT
compareAssignPrio (Full _)  Open      = LT
compareAssignPrio _         _         = EQ

newtype RoomNumber = RoomNumber Int

derive newtype instance eqRoomNumber :: Eq RoomNumber
derive newtype instance ordRoomNumber :: Ord RoomNumber

newtype BedNumber = BedNumber Int

derive newtype instance eqBedNumber :: Eq BedNumber
derive newtype instance ordBedNumber :: Ord BedNumber

type Bed =
  { roomNumber  :: RoomNumber
  , bedNumber   :: BedNumber
  , vacancy     :: Vacancy
  }

sortByPriority :: Array Bed -> Array Bed
sortByPriority = sortBy $ \b1 b2 -> compareAssignPrio b1.vacancy b2.vacancy

isSameRoom :: Bed -> Bed -> Boolean
isSameRoom b1 b2 = b1.roomNumber == b2.roomNumber

isSameBed :: Bed -> Bed -> Boolean
isSameBed b1 b2 = isSameRoom b1 b2 && b1.bedNumber == b2.bedNumber
