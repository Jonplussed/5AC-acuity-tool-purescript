module AcuityTool.Patient where

import Prelude

import AcuityTool.AccomCode (AccomCode (..))
import AcuityTool.Acuity (Acuity (..))

type Patient =
  { accomCode :: AccomCode
  , acuity    :: Acuity
  }

patient :: AccomCode -> Int -> Patient
patient c a =
  { accomCode: c
  , acuity: Acuity a
  }

priority :: Patient -> Patient -> Ordering
priority p q =
  compare q.acuity p.acuity <>
  compare q.accomCode p.accomCode
