module Chapter2.Exercise75
    (
        makeFromMagAng',
        makeFromMagAng,
        makeFromRealImag,
        Complex(realPart, imagPart, magnitude, angle)
    )
where

import Chapter1.Utilities (square)

-- | Implement the construtor makeFromMagAng in message-passing style. This
-- procedure should be analogous to the makeFromRealImag procedure given on
-- page 187.

makeFromMagAng' :: Double -> Double -> String -> Double
makeFromMagAng' m theta = dispatch
  where
    dispatch "realPart"  = m * cos theta
    dispatch "imagPart"  = m * sin theta
    dispatch "magnitute" = m
    dispatch "angle"     = theta
    dispatch _           = error "unknown operation"

-- | Note in Haskell, you might define such a structure as the record
data Complex = Complex { realPart  :: Double
                       , imagPart  :: Double
                       , magnitude :: Double
                       , angle     :: Double
                       }
    deriving stock (Show, Eq)

-- and then implement makeFromMagAng and makeFromRealImag to return instances:
makeFromMagAng :: Double -> Double -> Complex
makeFromMagAng m theta = Complex { realPart  = m * cos theta
                                 , imagPart  = m * sin theta
                                 , magnitude = m
                                 , angle     = theta
                                 }

makeFromRealImag :: Double -> Double -> Complex
makeFromRealImag real imag = Complex { realPart  = real
                                     , imagPart  = imag
                                     , magnitude = sqrt (square real + square imag)
                                     , angle     = atan (imag / real)
                                     }
