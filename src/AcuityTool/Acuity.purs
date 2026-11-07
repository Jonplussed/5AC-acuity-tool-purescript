module AcuityTool.Acuity where

import Prelude

import Data.Array (length, null)
import Data.Foldable (sum)
import Data.Int (round, toNumber)
import Data.Newtype (class Newtype, wrap, unwrap)

newtype Acuity = Acuity Int

derive instance newtypeAcuity :: Newtype Acuity _

derive newtype instance eqAcuity :: Eq Acuity
derive newtype instance ordAcuity :: Ord Acuity
derive newtype instance boundedAcuity :: Bounded Acuity
derive newtype instance semiringAcuity :: Semiring Acuity
derive newtype instance showAcuity :: Show Acuity

-- NOTE: Integer division is inconsistent across Purescript versions; this
-- solidifies the behavior.
average :: Array Acuity -> Acuity
average a | null a = Acuity 0
average a = wrap <<< round $ toNumber (unwrap $ sum a) / toNumber (length a)
