module AcuityTool.Bed where

import Prelude

import Data.Array (sortBy)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import AcuityTool.Patient as P

data Vacancy = Vacant | Occupied P.Patient

vacancyPrio :: Vacancy -> Vacancy -> Ordering
vacancyPrio (Occupied p1) (Occupied p2) = P.acuityPrio p1 p2
vacancyPrio Vacant        (Occupied _)  = LT
vacancyPrio Vacant        _             = EQ
vacancyPrio _             _             = GT

derive instance eqVacancy :: Eq Vacancy
derive instance genericVacancy :: Generic Vacancy _

instance showVacancy :: Show Vacancy where
  show = genericShow

newtype RoomNumber = RoomNumber Int

derive newtype instance eqRoomNumber :: Eq RoomNumber
derive newtype instance ordRoomNumber :: Ord RoomNumber
derive newtype instance showRoomNumber :: Show RoomNumber

newtype BedNumber = BedNumber Int

derive newtype instance eqBedNumber :: Eq BedNumber
derive newtype instance ordBedNumber :: Ord BedNumber
derive newtype instance showBedNumber :: Show BedNumber

type Bed =
  { roomNumber  :: RoomNumber
  , bedNumber   :: BedNumber
  , vacancy     :: Vacancy
  }

bedPrio :: Array Bed -> Array Bed
bedPrio = sortBy $ \b1 b2 -> vacancyPrio b1.vacancy b2.vacancy

isSameRoom :: Bed -> Bed -> Boolean
isSameRoom b1 b2 = b1.roomNumber == b2.roomNumber

isSameBed :: Bed -> Bed -> Boolean
isSameBed b1 b2 = isSameRoom b1 b2 && b1.bedNumber == b2.bedNumber
