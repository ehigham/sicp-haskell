module Chapter1.TExercise37 (tests) where
    import Chapter1.Exercise37 (calculatePhi)
    import TestTools (assertEquals)
    import Test.HUnit

    tPhi :: Test
    tPhi = TestCase (assertEquals
            "phi = 1.61803398875"
            0.0001
            1.61803398875
            (calculatePhi 10))

    tests :: Test
    tests = TestList [tPhi]