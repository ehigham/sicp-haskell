module Chapter2.TExercise6 (tests) where
    import Chapter2.Exercise6 (ChurchNumeral)
    import Test.HUnit

    tests :: Test
    tests = TestList [
        TestLabel "two plus two is four" (four ~=? two + two),
        TestLabel "minus one is three quick maths" (three ~=? four - one),
        TestLabel "two times three is six" (fromInteger 6 ~=? two * three),
        TestLabel "two > 1" (True ~=? two > one),
        TestLabel "two plus zero is two" (two ~=? two + zero),
        TestLabel "two times zero is zero" (zero ~=? two * zero),
        TestLabel "succ two is three" (three ~=? succ two)]
      where
        zero, one, two, three, four :: ChurchNumeral
        zero = fromInteger 0
        one = fromInteger 1
        two = fromInteger 2
        three = fromInteger 3
        four = fromInteger 4
