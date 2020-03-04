module Chapter1.TExercise19 (tests) where
    import Chapter1.Exercise19 (fib)
    import Test.HUnit

    tFib10To20 :: Test
    tFib10To20 = TestCase (assertEqual
        ("fib [10..20] ")
        ([55, 89, 144,233, 377, 610, 987, 1597, 2584, 4181, 6765] :: [Int])
        (fmap fib ([10..20] :: [Int])))

    tests :: Test
    tests = TestList [tFib10To20]