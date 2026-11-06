module AcuityTool.Acuity where

newtype Acuity = Acuity Int

derive newtype instance eqAcuity :: Eq Acuity
derive newtype instance ordAcuity :: Ord Acuity
derive newtype instance boundedAcuity :: Bounded Acuity
derive newtype instance semiringAcuity :: Semiring Acuity
derive newtype instance showAcuity :: Show Acuity
