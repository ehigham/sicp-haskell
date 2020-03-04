module Chapter1.TExercise17 (tests) where
    import Chapter1.Exercise17 (mult, fastMult)
    import Control.Monad (ap)
    import Test.HUnit

    testBothEqualTo :: Int -> Int -> Int -> Test
    testBothEqualTo b n res = TestList [
        TestCase(assertEqual ("mult "++inputs) (go mult) res),
        TestCase(assertEqual ("fastMult' "++inputs) (go fastMult) res)]
        where
            go f = f b n
            inputs = (show b)++" "++(show n)++" == "++(show res)

    tCorrectness :: Test
    tCorrectness = TestList[
        (testBothEqualTo 0 0 0),    -- 0 * 0 == 0
        (testBothEqualTo 2 5 10),   -- 2 * 5 == 10
        (testBothEqualTo 3 15 45),  -- 3 * 15 == 45
        (testBothEqualTo 6 7 42)]   -- 6 * 7 == 42

    tSameAnswer :: Test
    tSameAnswer = TestList $ [makeTest] `ap` xs `ap` ys
        where
            xs = ([1..3] :: [Int])
            ys = ([0..2] :: [Int])
            makeTest b n = TestCase (assertEqual
                (let inputs = show b ++ " " ++ show n 
                in "(mult "++inputs++") == (fastMult "++inputs++")")
                (mult b n)
                (fastMult b n))

    tests :: Test
    tests = TestList [tCorrectness, tSameAnswer]