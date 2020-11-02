module Chapter2.TExercise18 (tests) where
    import Chapter2.Exercise18 (reverse')

    import Test.HUnit

    tests :: Test
    tests = TestList [
        TestLabel "Empty lists" (mkTest []),
        TestLabel "singleton" (mkTest [1]),
        TestLabel "2 elements" (mkTest [1,2]),
        TestLabel "3 elements" (mkTest [1,2,3])
        ]
      where
        mkTest :: [Int] -> Test
        mkTest xs = TestCase $ assertEqual "" (reverse xs) (reverse' xs)
