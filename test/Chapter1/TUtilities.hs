module Chapter1.TUtilities (chapter1UtilitiesTests) where
    import Chapter1.Utilities (sqrt_)
    import Test.HUnit
    import Control.Monad (unless)

    assertEquals :: String  -- ^ The message prefix
                -> Double  -- ^ The maximum difference between expected and actual
                -> Double  -- ^ The expected value
                -> Double  -- ^ The actual value
                -> Assertion
    assertEquals preface delta expected actual = 
        unless (abs (expected - actual) < delta) (assertFailure msg)
        where msg = (if null preface then "" else preface ++ "\n") ++
                    "expected: " ++ show expected ++ "\n but got: " ++ show actual

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

    chapter1UtilitiesTests :: Test
    chapter1UtilitiesTests = TestList [tPowersOfTwo, tVsPrelude]


