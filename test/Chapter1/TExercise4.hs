module Chapter1.TExercise4 (tests) where
import Chapter1.Exercise4 (aPlusAbsB)
import Test.HUnit

tPositives, tNegativeA, tNegativeB, tZeros, tBothNegative :: Test

tPositives = TestCase (assertEqual
    "aPlusAbsB 1 2"
    (3 :: Int)
    (aPlusAbsB 1 2))

tNegativeA = TestCase (assertEqual
    "aPlusAbsB -1 2"
    (1 :: Int)
    (aPlusAbsB (-1) 2))

tNegativeB = TestCase (assertEqual
    "aPlusAbsB 1 -2"
    (3 :: Int)
    (aPlusAbsB 1 (-2)))

tZeros = TestCase (assertEqual
    "aPlusAbsB 0 0"
    (0 :: Int)
    (aPlusAbsB 0 0))

tBothNegative = TestCase (assertEqual
    "aPlusAbsB -1 -2"
    (1 :: Int)
    (aPlusAbsB (-1) (-2)))

tests :: Test
tests = TestList [tPositives, tNegativeA, tNegativeB, tZeros, tBothNegative]
