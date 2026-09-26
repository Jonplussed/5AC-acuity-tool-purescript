module AcuityTool.Bed where

import Prelude

import Data.Array (sortBy)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
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

type Bed =
  { roomNumber  :: RoomNumber
  , bedNumber   :: BedNumber
  , patient     :: Maybe P.Patient
  }

patientPrio :: Maybe P.Patient -> Maybe P.Patient -> Ordering
patientPrio (Just p1) (Just p2) = P.acuityPrio p1 p2
patientPrio Nothing   (Just _)  = LT
patientPrio Nothing   _         = EQ
patientPrio _         _         = GT

sortByPrio :: Array Bed -> Array Bed
sortByPrio = sortBy $ \b1 b2 -> patientPrio b1.patient b2.patient

isSameRoom :: Bed -> Bed -> Boolean
isSameRoom b1 b2 = b1.roomNumber == b2.roomNumber

isSameBed :: Bed -> Bed -> Boolean
isSameBed b1 b2 = isSameRoom b1 b2 && b1.bedNumber == b2.bedNumber
