module Chapter2.TExercise75 (tests) where

import Chapter2.Exercise75
    (
        makeFromMagAng,
        makeFromRealImag,
        Complex(magnitude, angle)
    )
import Test.HUnit

tests :: Test
tests = TestCase $ assertEqual
    "The real/imag and mag/angle representations are equivalent"
    x
    (makeFromMagAng (magnitude x) (angle x))
  where
    x = makeFromRealImag 3 5
