module Chapter1.TExercise7 (chapter1Exercise7Tests) where
    import Chapter1.Exercise6 as Exercise6(sqrt_)
    import Chapter1.Exercise7 as Exercise7(sqrt_)
    import TestTools (assertEquals)
    import Test.HUnit

    tPowersOfTwo, tVsPrelude :: Test

    tPowersOfTwo = TestCase (assertEqual
        "sqrt_ 4"
        (2.0 :: Double)
        (Exercise7.sqrt_ 4))

    tVsPrelude = TestCase (assertEquals
        "(sqrt_ 3) - (sqrt 3) < 0.001"
        0.001
        (Exercise7.sqrt_ 3)
        (sqrt 3))

    tSmallNumbers :: Test
-- | Use half x as the initial guess in sqrtIter
    tSmallNumbers = TestCase (assertEqual
        "Exercise6.sqrt_ failing at small numbers"
        (0.0005 :: Double)
        (Exercise6.sqrt_ 0.001))

    -- tLargeNumbers = TestCase (assertEqual
    --     "Exercise6.sqrt_ failing at large numbers"
    --     (5e99 :: Double)
    --     (Exercise6.sqrt_ 1e100))

    chapter1Exercise7Tests :: Test
    chapter1Exercise7Tests = TestList [
        tPowersOfTwo,
        tVsPrelude,
        tSmallNumbers]


