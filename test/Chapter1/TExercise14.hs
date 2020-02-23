module Chapter1.TExercise14 (tests) where
    import Chapter1.Exercise14 (countChange)
    import Test.HUnit

    tests :: Test
    tests = TestList[TestCase (assertEqual
        "countChange 100 == 292"
        (292 :: Int)
        (countChange 100)),
        TestCase (assertEqual
        "countChange 11 == 4"
        (4 :: Int)
        (countChange 11))]