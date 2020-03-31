module Chapter1.TExercise27 (tests) where
    import Chapter1.Exercise27 (isCarmichael)
    import Test.HUnit

    tIsCarmichael :: Test
    tIsCarmichael = TestCase (assertBool
        ("isCarmichael [561, ...] ")
        (and $ fmap isCarmichael carmichaels))
      where
        carmichaels = [561, 1105, 1729, 2465, 2821, 6601] :: [Int]

    tIsNotCarmichael :: Test
    tIsNotCarmichael = TestCase (assertBool
        ("(not . isCarmichael) [2, 3, 5, 7, 11]")
        (and $ fmap (not . isCarmichael) ([2, 3, 5, 7, 11] :: [Int])))

    tests :: Test
    tests = TestList [tIsCarmichael, tIsNotCarmichael]