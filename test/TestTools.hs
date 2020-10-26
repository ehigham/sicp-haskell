module TestTools (assertEquals, assertNotEqual, (@/=?), (~/=?)) where
    import Test.HUnit
    import Control.Monad (unless, when)

-- | "equality" test for rational numbers
    assertEquals :: String  -- ^ The message prefix
                -> Double  -- ^ The max relative difference between expected and actual
                -> Double  -- ^ The expected value
                -> Double  -- ^ The actual value
                -> Assertion
    assertEquals preface tolerance expected actual =
        unless ((relativeDifference expected actual) < tolerance) (assertFailure msg)
      where
        msg = (if null preface then "" else preface ++ "\n") ++
                "expected: " ++ show expected ++ "\n but got: " ++ show actual

    assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
    assertNotEqual preface x y = when (x == y) (assertFailure msg)
      where
        msg = (if null preface then "" else preface ++ "\n") ++
            "The values where the same: " ++ show x

    infix 1 @/=?, ~/=?
    (@/=?) :: (Eq a, Show a) => a -> a -> Assertion
    (@/=?) x y = assertNotEqual "" x y


    (~/=?) :: (Eq a, Show a) => a -> a -> Test
    (~/=?) = (TestCase .) . (@/=?)

    relativeDifference :: Double -> Double -> Double
    relativeDifference expected actual
        = abs (if isZero expected then actual else (1.0 - actual / expected))

    isZero :: Double -> Bool
    isZero = (< someSmallNumber) . abs where someSmallNumber = 1e-100

