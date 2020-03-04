module Chapter1.TExercise18 (tests) where
    import Chapter1.Exercise17 as Recursive(fastMult)
    import Chapter1.Exercise18 as Iterative(fastMult)
    import Control.Monad (ap)
    import Test.HUnit

    tSameAnswer :: Test
    tSameAnswer = TestList $ [makeTest] `ap` xs `ap` ys
        where
            xs = ([1..3] :: [Int])
            ys = ([0..2] :: [Int])
            makeTest b n = TestCase (assertEqual
                (let inputs = show b ++ " " ++ show n 
                in "(fastMult "++inputs++") == (fastMultIter "++inputs++")")
                (Recursive.fastMult b n)
                (Iterative.fastMult b n))

    tests :: Test
    tests = TestList [tSameAnswer]