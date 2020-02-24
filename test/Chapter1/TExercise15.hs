module Chapter1.TExercise15 (tests) where
    import Chapter1.Exercise15 (sine, p)
    import TestTools (assertEquals)
    import Test.HUnit

    tSine, tApplicationsOfP :: Test
    tSine = TestCase (assertEquals
        "sine 12.15 within 0.1 of sin 12.15"
        0.1
        (sin 12.15)
        (sine 12.15))

    tApplicationsOfP = TestCase (assertEquals
        "p applied 5 times in sine 12.15"
        1e-9
        (sine 12.15)
        (iterate p 0.05 !! 5))

    tests :: Test
    tests = TestList [tSine, tApplicationsOfP]
