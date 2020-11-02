{-# OPTIONS_GHC -Wno-type-defaults #-}
module Chapter2.TExercise20 (tests) where
    import Chapter2.Exercise20 (sameParity)
    import Test.HUnit

    tests :: Test
    tests = TestList [
        TestLabel "singleton list" ([1] ~=? (sameParity 1 :: [Integer])),
        TestLabel "odd numbers"    ([1, 3, 5, 7] ~=? (sameParity 1 2 3 4 5 6 7 :: [Integer])),
        TestLabel "even numbers"   ([2, 4, 6, 8] ~=? (sameParity 2 3 4 5 6 7 8 :: [Integer]))
        ]
