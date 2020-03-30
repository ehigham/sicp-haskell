module Chapter1.TExercise29 (tests) where
    import Chapter1.Exercise29 (simpson, integral)
    import Chapter1.Utilities (cube)
    import TestTools (assertEquals)
    import Test.HUnit

    tCube :: Test
    tCube = TestList [
        TestCase (assertEquals
            "abs ((simpson cube 0 1 2) - 0.25) < 0.001"
            0.001
            0.25
            (simpson cube 0 1 2)),
        TestCase (assertEquals
            "abs ((integral cube 0 1 0.01) - 0.25) < 0.001"
            0.001
            0.25
            (integral cube 0 1 0.01))]

    tCos :: Test
    tCos = TestList [
        TestCase (assertEquals
            "abs ((simpson cos 0 pi 8) - 0.0) < 0.001"
            0.001
            0.0
            (simpson cos 0 pi 8)),
        TestCase (assertEquals
            "abs ((integral cos 0 1 0.001) - 0.25) < 0.001"
            0.001
            0.0
            (integral cos 0 pi 0.001))]

    tests :: Test
    tests = TestList [tCube, tCos]