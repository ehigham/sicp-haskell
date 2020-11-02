{-# OPTIONS_GHC -Wno-type-defaults #-}
module Chapter2.TExercise17 (tests) where
    import Chapter2.Exercise17 (lastPair)

    import Data.Either (isLeft)
    import Control.Exception (SomeException, try, evaluate)
    import Test.HUnit (Test(..), Assertion, (~=?), assertBool)

    tests :: Test
    tests = TestList [
        TestLabel "from book" ([34] ~=? lastPair [23, 72, 149, 34]),
        TestLabel "singleton" ([1] ~=? lastPair [1]),
        TestLabel "empty list" (TestCase $ assertError "empty list should error" (lastPair []))]

    assertError :: String -> [Int] -> Assertion
    assertError msg xs = do
        result <- try (mapM evaluate xs) :: IO (Either SomeException [Int])
        assertBool msg (isLeft result)
