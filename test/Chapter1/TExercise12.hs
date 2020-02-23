module Chapter1.TExercise12 (tests) where
    import Chapter1.Exercise12 (pascal)
    import Test.HUnit

    makeTest :: Int -> Int -> Int -> Test
    makeTest m n expected = TestCase (assertEqual
        ("pascal " ++ show m ++ " " ++ show n)
        (expected)
        (pascal m n))

    tests :: Test
    tests = TestList [
        (makeTest 0 0 1),
        (makeTest 1 0 1),
        (makeTest 1 1 1),
        (makeTest 2 0 1),
        (makeTest 2 1 2),
        (makeTest 3 1 3),
        (makeTest 4 1 4),
        (makeTest 4 2 6)]
