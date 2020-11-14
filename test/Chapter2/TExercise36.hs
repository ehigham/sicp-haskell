module Chapter2.TExercise36 (tests) where

import Chapter2.Exercise36 (accumulateN)
import Test.HUnit

tests :: Test
tests = TestCase $ assertEqual
    "book example"
    ([22, 26, 30] :: [Integer])
    (accumulateN (+) 0 [[1..3], [4..6], [7..9], [10..12]])
