module Chapter2.TExercise1 (tests) where
    import Chapter2.Exercise1 (mkRat)
    import Test.HUnit

    tests :: Test
    tests = TestList [
        TestLabel "simplify" (onethird ~=? (mkRat 3 9)),
        TestLabel "negate" (negate onethird ~=? (mkRat 1 (negate 3))),
        TestLabel "add" (mkRat 2 3 ~=? (onethird + onethird)),
        TestLabel "multiply" (mkRat 1 9 ~=? (onethird * onethird)),
        TestLabel "subtract" (fromInteger 0 ~=? (onethird - onethird)),
        TestLabel "abs" (onethird ~=? abs (negate onethird)),
        TestLabel "zero == -zero" (fromInteger 0 ~=? negate (mkRat 0 1))]
      where
        onethird = mkRat 1 3
