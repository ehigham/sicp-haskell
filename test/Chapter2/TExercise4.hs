module Chapter2.TExercise4 (tests) where
    import Chapter2.Exercise4 (cons, car, cdr)
    import Test.HUnit

    tests :: Test
    tests = TestList [
        TestLabel "car" (0 ~=? car (cons (0 :: Int) tail')),
        TestLabel "cdr" (tail' ~=? cdr (cons (0 :: Int) tail'))]
      where
        tail' = cons (1 :: Int) (2 :: Int)
