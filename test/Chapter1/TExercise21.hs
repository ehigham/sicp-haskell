module Chapter1.TExercise21 (tests) where
    import Chapter1.Exercise21 (smallestDivisor)
    import Test.HUnit

    t1To10, tVsLiterature :: Test
    t1To10 = TestCase (assertEqual
        ("smallestDivisor [1..10] ")
        ([1,2,3,2,5,2,7,2,3,2] :: [Int])
        (fmap smallestDivisor ([1..10] :: [Int])))

    tVsLiterature = TestCase (assertEqual
        ("smallestDivisor [199, 1999, 19999] ")
        ([199, 1999, 7] :: [Int])
        (fmap smallestDivisor ([199, 1999, 19999] :: [Int])))


    tests :: Test
    tests = TestList [t1To10, tVsLiterature]