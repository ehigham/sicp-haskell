module Chapter1.TExercise1 (chapter1_exercise1_tests) where
    import Chapter1.Exercise1
    import Test.HUnit

    tAdd :: Test
    tAdd = TestCase (assertEqual "add [1, 2]" (3 :: Int) (sum [1, 2])) 

    chapter1_exercise1_tests :: Test
    chapter1_exercise1_tests = TestList [tAdd]