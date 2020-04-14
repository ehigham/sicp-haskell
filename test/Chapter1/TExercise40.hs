module Chapter1.TExercise40 (tests) where
    import Chapter1.Exercise40 (cubic, newtonsMethod)
    import TestTools (assertEquals)
    import Test.HUnit

    tCubic :: Test
    tCubic = TestCase (assertEquals
            "newtonsMethod x^3 - 4.x^2 + 6.x - 24 ~ 4.0"
            0.0001
            ((4.0 :: Double))
            (newtonsMethod f 0.0))
      where
        f = cubic (negate 4) 6 (negate 24)

    tests :: Test
    tests = TestList [tCubic]
