module Chapter1.TExercise31 (tests) where
    import Chapter1.Exercise31 (product, factorial)
    import Chapter1.Utilities (square)
    import Prelude hiding (product)
    import Test.HUnit

    tFactorial :: Test
    tFactorial = TestList[
        TestCase (assertEqual
            "factorial 10 = 3628800"
            (3628800 :: Int)
            (factorial 10)),
        TestCase (assertEqual
            "factorial 0 = 1"
            (1 :: Int)
            (factorial 0))]

    tProductSquares :: Test
    tProductSquares = TestCase (assertEqual
            "product square 1 (+1) 10 = 13168189440000"
            (13168189440000 :: Int)
            (product square 1 (+1) 10))

    tests :: Test
    tests = TestList [tFactorial, tProductSquares]