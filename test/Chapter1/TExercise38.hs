module Chapter1.TExercise38 (tests) where
    import Chapter1.Exercise38 (calculateExp)
    import TestTools (assertEquals)
    import Test.HUnit

    tExp :: Test
    tExp = TestCase (assertEquals
            "phi = 1.61803398875"
            0.0001
            (exp (1 :: Double))
            (calculateExp 8))

    tests :: Test
    tests = TestList [tExp]