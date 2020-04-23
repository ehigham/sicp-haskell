module Chapter1.TExercise45 (tests) where
    import Chapter1.Exercise45 (rootN)
    import TestTools (assertEquals)
    import Test.HUnit

    tests :: Test
    tests = TestList $ fmap mkRootNTest ns
      where
        mkRootNTest n = TestCase (assertEquals
            ("(rootN " ++ show n ++ " 2)^" ++ show n ++ " == 2")
            0.0001
            (2 :: Double)
            (rootN (fromIntegral n) (2 :: Double) ^ n))
        ns = [2..10] :: [Int]
