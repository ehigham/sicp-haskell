module Chapter1.TExercise30 (tests) where
    import Chapter1.Exercise30 (sum)
    import Chapter1.Utilities (square)
    import Prelude hiding (sum)
    import Test.HUnit

    tSum :: Test
    tSum = TestCase (assertEqual
            "sum id 1 (+1) 10 = 55"
            (55 :: Int)
            (sum id 1 (+1) 10))

    tSumSquares :: Test
    tSumSquares = TestCase (assertEqual
            "sum square 1 (+1) 15 = 1240"
            (1240 :: Int)
            (sum square 1 (+1) 15))

    tests :: Test
    tests = TestList [tSum, tSumSquares]