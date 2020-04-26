module Chapter2.TExercise2 (tests) where
    import Chapter2.Exercise2 (Point (Point), Segment (Segment), midpoint)
    import Test.HUnit

    tests :: Test
    tests = let origin = (Point 0 0) in TestLabel "midpoint"
        ((Point 1 1) ~=? midpoint (Segment origin (Point 2 2)))
