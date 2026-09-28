module AcuityTool.Bed where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (decimal, toStringAs)
import Data.Show.Generic (genericShow)

import AcuityTool.Patient as P

data Bed = Bed Int Int

derive instance genericBed :: Generic Bed _

instance eqBed :: Eq Bed where
  eq (Bed r1 b1) (Bed r2 b2) = r1 == r2 && b1 == b2

instance ordBed :: Ord Bed where
  compare (Bed r1 b1) (Bed r2 b2) = compare r1 r2 <> compare b1 b2

instance showBed :: Show Bed where
  show = genericShow

isSameRoom :: Bed -> Bed -> Boolean
isSameRoom (Bed r1 _) (Bed r2 _) = r1 == r2

label :: Bed -> String
label (Bed r b) = toStringAs decimal r <> "-" <> toStringAs decimal b
