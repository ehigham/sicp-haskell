module Chapter2.TExercise34 (tests) where

import Chapter2.Exercise34 (horner)
import Test.HUnit

tests :: Test
tests = TestList [
    TestLabel "f x = c" $ 5 ~=? (horner [5] (negate 1) :: Integer),
    TestLabel "f x = 2x+3" $ 7 ~=? (horner [3, 2] 2 :: Integer),
    TestLabel "f x = 3x^2 + 4" $ 31 ~=? (horner [4, 0, 3] (negate 3) :: Integer),
    TestLabel "book example" $ let x = 2 :: Integer in f x ~=? horner coeffs x
    ]
  where
    f x = 1 + 3*x + 5*x^(3 :: Integer) + x^(5 :: Integer)
    coeffs = [1, 3, 0, 5, 0, 1]
