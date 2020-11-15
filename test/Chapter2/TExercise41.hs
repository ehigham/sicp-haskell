module Chapter2.TExercise41 (tests) where

import Chapter2.Exercise41 (orderedTriples)
import Test.HUnit

tests :: Test
tests = TestList [
    orderedTriples 0 0 ~=? [],
    orderedTriples 3 3 ~=? [(1,1,1)],
    orderedTriples 3 5 ~=? [(1,1,3),(1,2,2)],
    orderedTriples 5 12 ~=? [(2,5,5),(3,4,5),(4,4,4)]
    ]
