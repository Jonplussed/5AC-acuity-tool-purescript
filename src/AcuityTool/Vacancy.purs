module AcuityTool.Vacancy where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Vacancy = Vacant | Occupied

derive instance genericVacancy :: Generic Vacancy _
derive instance eqVacancy :: Eq Vacancy

instance showVacancy :: Show Vacancy where
  show = genericShow
