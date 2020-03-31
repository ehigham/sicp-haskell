module Chapter1.TExercise35 (tests) where
    import Chapter1.Exercise35 (fixed, phi)
    import TestTools (assertEquals)
    import Control.Monad (liftM2)
    import Test.HUnit

    tFromBook :: Test
    tFromBook = TestList [
        TestCase (assertEquals
            "abs (fixed cos 1) - 0.73908) < 0.001"
            0.001
            0.7390822985224023
            (fixed cos 1)),
        TestCase (assertEquals
            "abs (fixed (sin + cos) 1) - 1.25873) < 0.001"
            0.001
            1.2587315962971173
            (fixed (liftM2 (+) sin cos) 1))]

    tPhi :: Test
    tPhi = TestCase (assertEquals
            "phi = 1.61803398875"
            0.001
            1.61803398875
            phi)

    tests :: Test
    tests = TestList [tFromBook, tPhi]