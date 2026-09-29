module Test.AcuityTool.SpecHelpers where

import Prelude

import Data.Maybe (Maybe)

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl

-- TODO: remove legacy helper fn
bed :: Int -> Int -> B.Bed
bed = B.Bed

patient :: Pa.AccomCode -> Int -> Pa.Patient
patient s n = { accom: s, acuity: Pa.Acuity n }

place :: B.Bed -> Maybe Pa.Patient -> Pl.Placement
place b p = { bed: b, patient: p }
