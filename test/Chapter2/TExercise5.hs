module Chapter2.TExercise5 (tests) where
    import Chapter2.Exercise5 (cons, car, cdr)
    import Test.HUnit

    tests :: Test
    tests = TestList [
        TestLabel "car (cons 0 0)" (0 ~=? car (cons 0 0)),
        TestLabel "cdr (cons 0 0)" (0 ~=? cdr (cons 0 0)),
        TestLabel "car (cons 1 0)" (1 ~=? car (cons 1 0)),
        TestLabel "cdr (cons 1 0)" (0 ~=? cdr (cons 1 0)),
        TestLabel "car (cons 0 1)" (0 ~=? car (cons 0 1)),
        TestLabel "cdr (cons 0 1)" (1 ~=? cdr (cons 0 1)),
        TestLabel "car (cons 3 2)" (3 ~=? car (cons 3 2)),
        TestLabel "cdr (cons 3 2)" (2 ~=? cdr (cons 3 2))]
