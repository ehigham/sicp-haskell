module Chapter1.TExercise6 (chapter1Exercise6Tests) where
    import Chapter1.Exercise6 (sqrt')
    import TestTools (assertEquals)
    import Test.HUnit

    tPowersOfTwo, tVsPrelude :: Test
    tPowersOfTwo = TestCase (assertEqual
        "sqrt' 4"
        (2.0 :: Double)
        (sqrt' 4))

    tVsPrelude = TestCase (assertEquals
        "(sqrt' 3) - (sqrt 3) < 0.001"
        0.001
        (sqrt' 3)
        (sqrt 3))

    chapter1Exercise6Tests :: Test
    chapter1Exercise6Tests = TestList [tPowersOfTwo, tVsPrelude]


