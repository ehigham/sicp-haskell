module Chapter1.TExercise6 (chapter1Exercise6Tests) where
    import Chapter1.Exercise6 (sqrt_)
    import TestTools (assertEquals)
    import Test.HUnit

    tPowersOfTwo, tVsPrelude :: Test
    tPowersOfTwo = TestCase (assertEqual
        "sqrt_ 4"
        (2.0 :: Double)
        (sqrt_ 4))

    tVsPrelude = TestCase (assertEquals
        "(sqrt_ 3) - (sqrt 3) < 0.001"
        0.001
        (sqrt_ 3)
        (sqrt 3))

    chapter1Exercise6Tests :: Test
    chapter1Exercise6Tests = TestList [tPowersOfTwo, tVsPrelude]


