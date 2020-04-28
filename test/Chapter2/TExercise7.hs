module Chapter2.TExercise7 (tests) where
    import Chapter1.Utilities (square)
    import Chapter2.Exercise7 (mkInterval, lower, upper, width)
    import Test.HUnit

    tests :: Test
    tests = TestList [
        TestLabel "lower" (0 ~=? lower (mkInterval 0 1)),
        TestLabel "upper" (1 ~=? upper (mkInterval 0 1)),
        TestLabel "add" (mkInterval 0 0 ~=? (mkInterval (negate 1) 1) + (mkInterval 1 (negate 1))),
        TestLabel "negate" (mkInterval (negate 1) (negate 1) ~=? negate (mkInterval 1 1)),
        TestLabel "subtract" (mkInterval (negate 2) 2 ~=? (mkInterval (negate 1) 1) - (mkInterval 1 (negate 1))),
        TestLabel "multiply" (mkInterval 4 9 ~=? square (mkInterval 2 3)),
        TestLabel "recip" (mkInterval (1/4) (1/2) ~=? recip (mkInterval 2 4)),
--        TestLabel "divide" (mkInterval 2 3 ~=? (mkInterval 4 9) / (mkInterval 2 3)),
        TestLabel "width" (3 ~=? width (mkInterval (negate 1) 5))]