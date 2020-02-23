module Chapter1.TExercise8 (tests) where
    import Chapter1.Exercise8 (cbrt')
    import TestTools (assertEquals)
    import Test.HUnit

    tThreeCubed, tFiveCubed, tVsPrelude :: Test
    tThreeCubed = TestCase (assertEquals
        "(cbrt' 27) == 3"
        0.001
        (3.0 :: Double)
        (cbrt' 27))

    tFiveCubed = TestCase (assertEquals
        "(cbrt' 125) == 5"
        0.001
        (5.0 :: Double)
        (cbrt' 125))    

    tVsPrelude = TestCase (assertEquals
        "(cbrt' 3) - (cbrt 3) < 0.001"
        0.001
        (3.0 ** (1.0/3.0))
        (cbrt' 3))

    tests :: Test
    tests = TestList [tThreeCubed, tFiveCubed, tVsPrelude]


