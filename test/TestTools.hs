module TestTools (assertEquals) where
    import Test.HUnit
    import Control.Monad (unless)

-- | "equality" test for rational numbers
    assertEquals :: String  -- ^ The message prefix
                -> Double  -- ^ The maximum difference between expected and actual
                -> Double  -- ^ The expected value
                -> Double  -- ^ The actual value
                -> Assertion
    assertEquals preface delta expected actual = 
        unless (abs (expected - actual) < delta) (assertFailure msg)
        where msg = (if null preface then "" else preface ++ "\n") ++
                    "expected: " ++ show expected ++ "\n but got: " ++ show actual
                    