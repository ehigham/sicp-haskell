module Chapter1.TExercise28 (tests) where
    import Chapter1.Exercise28 (isPrime)
    import Control.Monad (unless)
    import Test.HUnit

    assertBoolM :: String -> IO Bool -> Assertion
    assertBoolM msg mb = mb >>= \b -> unless b (assertFailure msg)

    tCarmichaels :: Test
    tCarmichaels = TestCase (assertBoolM
        "isPrime [561, ...]"
        (sequence (fmap isPrime carmichaels) >>= return . not . or))
      where
        carmichaels = [561, 1105, 1729, 2465, 2821, 6601] :: [Int]

    tPrimes :: Test
    tPrimes = TestCase (assertBoolM
        "isPrime [2, 3, 5, ...]"
        (sequence (fmap isPrime primes) >>= return . and))
      where
        primes = [2,3,5,7,11,13,17,19,23,29] :: [Int]

    tests :: Test
    tests = TestList [tCarmichaels, tPrimes]