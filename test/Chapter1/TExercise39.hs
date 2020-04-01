module Chapter1.TExercise39 (tests) where
    import Chapter1.Exercise39 (tanCF)
    import TestTools (assertEquals)
    import Test.HUnit

    tTangent :: Test
    tTangent = TestList $ fmap makeTest xs
        where
            xs = [(negate pi/12), (pi/16), (pi/8), (pi/4)] :: [Double]
            makeTest x = TestCase (assertEquals
                ("tan " ++ show x)
                0.001
                (tan x)
                (tanCF x 8))

    tests :: Test
    tests = TestList [tTangent]