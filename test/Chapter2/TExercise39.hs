module Chapter2.TExercise39 (tests) where

import Chapter2.Exercise39 (reverse', reverse'')
import Test.HUnit

tests :: Test
tests = TestList [
    TestLabel "Empty lists" $ mkTest [],
    TestLabel "singleton" $ mkTest [1],
    TestLabel "many elements" $ mkTest [1..10]
    ]
  where
    mkTest :: [Int] -> Test
    mkTest xs = reverse' xs ~=? reverse'' xs
