module Chapter1.TExercise16 (tests) where
    import Chapter1.Exercise16 (fastExpt, fastExpt')
    import Control.Monad (ap)
    import Test.HUnit

    

    testBothEqualTo :: Int -> Int -> Int -> Test
    testBothEqualTo b n res = TestList [
        TestCase(assertEqual ("fastExpt "++inputs) (go fastExpt) res),
        TestCase(assertEqual ("fastExpt' "++inputs) (go fastExpt') res)]
        where
            go f = f b n
            inputs = (show b)++" "++(show n)++" == "++(show res)

    tCorrectness :: Test
    tCorrectness = TestList[
        (testBothEqualTo 3 0 1),  -- 3 ^ 0 == 1
        (testBothEqualTo 3 1 3),  -- 3 ^ 1 == 2
        (testBothEqualTo 3 2 9),  -- 3 ^ 2 == 9
        (testBothEqualTo 3 3 27)] -- 3 ^ 3 == 9

    tSameAnswer :: Test
    tSameAnswer = TestList $ [makeTest] `ap` bases `ap` exponents
        where
            bases = ([1..3] :: [Int])
            exponents = ([0..2] :: [Int])
            makeTest b n = TestCase (assertEqual
                (let inputs = show b ++ " " ++ show n 
                in "(fastExpt'"++inputs++") == (fastExpt' "++inputs++")")
                (fastExpt b n)
                (fastExpt' b n))

    tests :: Test
    tests = TestList [tCorrectness, tSameAnswer]