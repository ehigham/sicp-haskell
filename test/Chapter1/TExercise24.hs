module Chapter1.TExercise24 (tests) where
    import Chapter1.Exercise24 (fastPrime)
    import System.Random (mkStdGen)
    import Test.HUnit

    isPrime :: Int -> Bool
    isPrime = fastPrime (mkStdGen 0) 10

    tPrimes :: Test
    tPrimes = TestCase (assertEqual
        ("isPrime [1..] ")
        ([2,3,5,7,11,13,17,19,23,29] :: [Int])
        (take 10 [x | x <- [1..], isPrime x]))

    tests :: Test
    tests = TestList [tPrimes]