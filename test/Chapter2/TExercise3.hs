module Chapter2.TExercise3 (tests) where
    import Chapter2.Exercise2 (Point (Point), Segment (Segment))
    import Chapter2.Exercise3 (area, perimeter, Rectangle (..), AltRectangle (..))
    import Test.HUnit

    tests :: Test
    tests = TestList [
        TestLabel "rect area" (15 ~=? area (mkRect 5 3)),
        TestLabel "rect perimeter" (16 ~=? perimeter (mkRect 5 3)),
        TestLabel "alt rect area" (15 ~=? area (mkAltRect 5 3)),
        TestLabel "alt rect perimeter" (16 ~=? perimeter (mkAltRect 5 3))]
      where
        mkRect x y = Rectangle (Segment origin (Point x 0)) (Segment origin (Point y 0))
        mkAltRect x y = AltRectangle origin (Point x y)
        origin = Point 0 0
