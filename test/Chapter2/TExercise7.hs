module Chapter2.TExercise7 (tests) where
    import Chapter2.Exercise7 (mkInterval, mkCentreWidth, mkCentrePercent, lower, upper, width)
    import Test.HUnit

    multiplyTests :: Test
    multiplyTests = TestList [
        TestLabel "(+, +)" (mkInterval (       10) (       21) ~=? mkInterval (       2) (       3) * mkInterval (       5) (       7)),
        TestLabel "(+, =)" (mkInterval (negate 15) (       21) ~=? mkInterval (       2) (       3) * mkInterval (negate 5) (       7)),
        TestLabel "(+, -)" (mkInterval (negate 21) (negate 10) ~=? mkInterval (       2) (       3) * mkInterval (negate 7) (negate 5)),
        TestLabel "(=, +)" (mkInterval (negate 14) (       21) ~=? mkInterval (negate 2) (       3) * mkInterval (       5) (       7)),
        TestLabel "(=, =)" (mkInterval (negate 15) (       21) ~=? mkInterval (negate 2) (       3) * mkInterval (negate 5) (       7)),
        TestLabel "(=, -)" (mkInterval (negate 21) (       14) ~=? mkInterval (negate 2) (       3) * mkInterval (negate 7) (negate 5)),
        TestLabel "(-, +)" (mkInterval (negate 21) (negate 10) ~=? mkInterval (negate 3) (negate 2) * mkInterval (       5) (       7)),
        TestLabel "(-, =)" (mkInterval (negate 21) (       15) ~=? mkInterval (negate 3) (negate 2) * mkInterval (negate 5) (       7)),
        TestLabel "(-, -)" (mkInterval (       10) (       21) ~=? mkInterval (negate 3) (negate 2) * mkInterval (negate 7) (negate 5))]

    tests :: Test
    tests = TestList [
        TestLabel "lower" (0 ~=? lower (mkInterval 0 1)),
        TestLabel "upper" (1 ~=? upper (mkInterval 0 1)),
        TestLabel "add" (mkInterval 0 0 ~=? (mkInterval (negate 1) 1) + (mkInterval 1 (negate 1))),
        TestLabel "negate" (mkInterval (negate 1) (negate 1) ~=? negate (mkInterval 1 1)),
        TestLabel "subtract" (mkInterval 0 0 ~=? (mkInterval (negate 1) 1) - (mkInterval 1 (negate 1))),
        TestLabel "multiply" multiplyTests,
        TestLabel "recip" (mkInterval (1/4) (1/2) ~=? recip (mkInterval 2 4)),
        TestLabel "divide" (mkInterval (1/4) 1 ~=? (mkInterval 2 4) / (mkInterval 4 8)),
        TestLabel "width" (3 ~=? width (mkInterval (negate 1) 5)),
        TestLabel "mkCentreWidth" (mkInterval 3.35 3.65 ~=? (mkCentreWidth 3.5 0.15)),
        TestLabel "mkCentrePercent" (mkInterval 2.983 3.297 ~=? (mkCentrePercent 3.14 5))]