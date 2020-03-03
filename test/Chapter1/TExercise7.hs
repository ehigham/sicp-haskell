module Chapter1.TExercise7 (chapter1Exercise7Tests) where
    import Chapter1.Exercise6 as Exercise6(sqrt')
    import Chapter1.Exercise7 as Exercise7(sqrt')
    import TestTools (assertEquals)
    import Test.HUnit

    tPowersOfTwo, tVsPrelude :: Test

    tPowersOfTwo = TestCase (assertEqual
        "sqrt' 4"
        (2.0 :: Double)
        (Exercise7.sqrt' 4))

    tVsPrelude = TestCase (assertEquals
        "(sqrt' 3) - (sqrt 3) < 0.001"
        0.001
        (Exercise7.sqrt' 3)
        (sqrt 3))

    tSmallNumbers :: Test
-- | Use halve x as the initial guess in sqrtIter
    tSmallNumbers = TestCase (assertEqual
        "Exercise6.sqrt' failing at small numbers"
        (0.0005 :: Double)
        (Exercise6.sqrt' 0.001))

-- | Uncomment if you like waiting 
    -- tLargeNumbers :: Test
    -- tLargeNumbers = TestCase (assertEqual
    --     "Exercise6.sqrt' failing at large numbers"
    --     (5e99 :: Double)
    --     (Exercise6.sqrt' 1e100))

    tSmallNumbersImproved :: Test
-- | As before, use halve x as the initial guess in sqrtIter
-- | Test case for improved goodEnough
    tSmallNumbersImproved = TestCase (assertEquals
        "(sqrt' 0.0001) - (sqrt 0.0001) < 0.001"
        0.001
        (Exercise7.sqrt' 0.0001)
        (sqrt 0.0001))

    tLargeNumbersImproved :: Test
    tLargeNumbersImproved = TestCase (assertEquals
        "(sqrt' 1e100) - (sqrt 1e100) < 0.001"
        0.001
        (Exercise7.sqrt' 1e100)
        (sqrt 1e100))

    chapter1Exercise7Tests :: Test
    chapter1Exercise7Tests = TestList [
        tPowersOfTwo,
        tVsPrelude,
        tSmallNumbers,
        -- tLargeNumbers,
        tSmallNumbersImproved,
        tLargeNumbersImproved]


