module Chapter1.TExercise43 (tests) where
    import Chapter1.Exercise43 (repeated)
    import Chapter1.Utilities (square)
    import Test.HUnit

    tests :: Test
    tests = TestCase (assertEqual
        "(repeated square 2) 5 = 625"
        (625 :: Int)
        ((repeated square 2) 5))

