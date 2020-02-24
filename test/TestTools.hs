module TestTools (assertEquals) where
    import Test.HUnit
    import Control.Monad (unless)

-- | "equality" test for rational numbers
    assertEquals :: String  -- ^ The message prefix
                -> Double  -- ^ The max relative difference between expected and actual
                -> Double  -- ^ The expected value
                -> Double  -- ^ The actual value
                -> Assertion
    assertEquals preface tolerance expected actual = 
        unless ((relativeDifference expected actual) < tolerance) (assertFailure msg)
        where msg = (if null preface then "" else preface ++ "\n") ++
                    "expected: " ++ show expected ++ "\n but got: " ++ show actual

    relativeDifference :: Double -> Double -> Double
    relativeDifference expected actual
        = abs (if isZero expected then actual else (1.0 - actual / expected))

    isZero :: Double -> Bool
    isZero = (< someSmallNumber) . abs where someSmallNumber = 1e-100

                    