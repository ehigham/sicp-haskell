module Chapter2.TExercise2 (tests) where
    import Chapter2.Exercise2 (Point (Point), Segment (Segment), midpoint, norm)
    import Test.HUnit

    tests :: Test
    tests = TestList [
        TestLabel "midpoint" ((Point 1 1) ~=? midpoint (Segment origin (Point 2 2))),
        TestLabel "norm" (5.0 ~=? norm (Segment origin (Point 3 4)))]
      where
        origin = (Point 0 0)
