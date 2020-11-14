module Chapter2.TExercise37 (tests) where

import Chapter2.Exercise37
import Test.HUnit


tests :: Test
tests = TestList [
    TestLabel "dotProduct" $ (14 :: Int) ~=? dotProduct [1..3] [1..3],
    TestCase $ assertEqual
        "tesing the dot product of a matrix and a vector"
        ([23, 23, 23] :: [Int])
        (dotProductM mat [1..3]),
    TestCase $ assertEqual
        "testing matrix multiplica\tion"
        ([[50,50,50], [60,60,60], [70,70,70]] ::[[Int]])
        (matmul mat (replicate 3 [5..7]))
    ]
  where
    mat = replicate 3 [2, 3, 5]


