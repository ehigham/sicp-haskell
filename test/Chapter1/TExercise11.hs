module Chapter1.TExercise11 (tests) where
    import Chapter1.Exercise11 (f, f')
    import Test.HUnit

    makeTest :: Int -> Test
    makeTest n = TestCase (assertEqual
        (let value = show n in "(f'" ++ value ++ ") == (f " ++ value ++ ")")
        (f n)
        (f' n))

    tests :: Test
    tests = TestList . fmap makeTest $ inputs
        where inputs = ([1..10] :: [Int])
