module Chapter1.TExercise3 (chapter1Exercise3Tests) where
    import Chapter1.Exercise3 (sumLargestSquares)
    import Test.HUnit

    tBasic, tOneNegative, tNegatives :: Test
    tBasic = TestCase (assertEqual
        "sumLargestSquares 1 2 3"
        (13 :: Int)
        (sumLargestSquares 1 2 3)) 

    tOneNegative = TestCase (assertEqual
        "sumLargestSquares 1 -2 3"
        (10 :: Int)
        (sumLargestSquares 1 (-2) 3)) 
    
    tNegatives = TestCase (assertEqual
        "sumLargestSquares -1 -2 -3"
        (5 :: Int)
        (sumLargestSquares (-1) (-2) (-3)))

    chapter1Exercise3Tests :: Test
    chapter1Exercise3Tests = TestList [tBasic, tOneNegative, tNegatives]