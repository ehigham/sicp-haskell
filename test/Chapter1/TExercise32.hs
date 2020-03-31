module Chapter1.TExercise32 (tests) where
    import Chapter1.Exercise32 (sum, product)
    import Prelude hiding (sum, product)
    import Test.HUnit

    tSum :: Test
    tSum = TestCase (assertEqual
            "sum id 1 (+1) 10 = 55"
            (55 :: Int)
            (sum id 1 (+1) 10))

    tProduct :: Test
    tProduct = TestCase (assertEqual
            "product id 1 (+1) 10 = 3628800"
            (3628800 :: Int)
            (product id 1 (+1) 10))

    tests :: Test
    tests = TestList [tSum, tProduct]