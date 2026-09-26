module AcuityTool.Assignment
  ( PatientCount(..)
  , Assignment
  , empty
  , assign
  , maxStatus
  , totalAcuity
  , patientCount
  ) where

import Prelude

import Data.Maybe (maybe)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import Data.Array as Arr
import AcuityTool.Patient as P
import AcuityTool.Bed as B

newtype PatientCount = PatientCount Int

derive newtype instance eqPatientCount :: Eq PatientCount
derive newtype instance ordPatientCount :: Ord PatientCount
derive newtype instance semiringPatientCount :: Semiring PatientCount
derive newtype instance showPatientCount :: Show PatientCount

type Assignment =
  { beds    :: Array B.Bed
  , status  :: P.Status
  , acuity  :: P.Acuity
  }

empty :: Assignment
empty = { beds: [] , status: P.MS, acuity: P.Acuity 0 }

assign :: Assignment -> B.Bed -> Assignment
assign a b =
  { beds:   Arr.snoc a.beds b
  , status: maxStatus a b
  , acuity: totalAcuity a b
  }

maxStatus :: Assignment -> B.Bed -> P.Status
maxStatus a b = maybe a.status (max a.status <<< _.status) b.patient

totalAcuity :: Assignment -> B.Bed -> P.Acuity
totalAcuity a b = maybe a.acuity (add a.acuity <<< _.acuity) b.patient

patientCount :: Assignment -> B.Bed -> PatientCount
patientCount a b = PatientCount $ maybe l (\_ -> l + 1) b.patient
  where l = Arr.length a.beds
