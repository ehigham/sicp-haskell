module Chapter1.TExercise9 (tests) where
    import Chapter1.Exercise9 (add', add'')
    import Test.HUnit

    xs :: [Int]
    xs = [0..10]

    addWith :: (Int -> Int -> Int) -> [Int] -> Int
    addWith f = foldl f (0 :: Int)

    tAdd', tAdd'' :: Test
    tAdd' = TestCase(assertEqual "add' 4 5" (9 :: Int) (add' 4 5))
    tAdd'' = TestCase(assertEqual "add'' 4 5" (9 :: Int) (add'' 4 5))

-- | Test associativity
    tAdd'Assoc, tAdd''Assoc :: Test
    tAdd'Assoc = TestCase(assertEqual "(add' 4 5)" (add' 4 5) (add' 5 4))
    tAdd''Assoc = TestCase(assertEqual "(add'' 4 5)" (add'' 4 5) (add'' 5 4))

    tSameSum :: Test
    tSameSum
        = TestCase(assertEqual "add' 1 2" (addWith add' xs) (addWith add' xs))

    tests :: Test
    tests = TestList [
        tAdd',
        tAdd'',
        tAdd'Assoc,
        tAdd''Assoc,
        tSameSum]
