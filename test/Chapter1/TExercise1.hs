module Chapter1.TExercise1 (chapter1Exercise1Tests) where
    import Test.HUnit

    tAdd :: Test
    tAdd = TestCase (assertEqual "add [1, 2]" (3 :: Int) (sum [1, 2])) 

    chapter1Exercise1Tests :: Test
    chapter1Exercise1Tests = TestList [tAdd]