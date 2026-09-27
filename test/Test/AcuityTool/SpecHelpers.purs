module Test.AcuityTool.SpecHelpers where

import Prelude

import Data.Maybe (Maybe)

import AcuityTool.Bed as B
import AcuityTool.Patient as Pa
import AcuityTool.Placement as Pl

bed :: Int -> Int -> B.Bed
bed r b = B.Bed (B.RoomNumber r) (B.BedNumber b)

patient :: Pa.Status -> Int -> Pa.Patient
patient s n = { status: s, acuity: Pa.Acuity n }

place :: B.Bed -> Maybe Pa.Patient -> Pl.Placement
place b p = { bed: b, patient: p }
