module Chapter1.TExercise41 (tests) where
    import Chapter1.Exercise41 (double)
    import Test.HUnit

    tInc, tExercise :: Test
    tInc = TestCase (assertEqual
            "double (+1) 1 = 3"
            (3 :: Int)
            (double (+1) 1))

    tExercise = TestCase (assertEqual
            "(double (double double)) (+1) 1 = 3"
            (21 :: Int)
            ((double (double double)) (+1) (5 :: Int)))

    tests :: Test
    tests = TestList [tInc, tExercise]
