module Chapter2.TExercise32 (tests) where
    import Chapter2.Exercise32 (subsets)
    import Test.HUnit

    tests :: Test
    tests = TestCase $
      assertEqual
        "sets of subsets form the book"
        [[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]]
        (subsets [1 .. 3] :: [[Integer]])

