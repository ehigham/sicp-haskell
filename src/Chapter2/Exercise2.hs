module Chapter2.Exercise2 (
    Point (..),
    Segment (..),
    midpoint,
    norm
) where
    import Chapter1.Utilities (average, square)
    import Data.Function (on)
-- | Consider the propblem of representing line segments in a plane. Each
-- | segment is represented as a pair of points: a starting point and an ending
-- | point. Define a constructor `MkSegment` and selectors `begin` and `end`
-- | that sefine the representation of segments in terms of points. Furthermore,
-- | a point can be represented as a pair of numbers: the x-coordinate and the
-- | y-coordinate. Accordingly, specify a constructor `MkPoint` and selectors
-- | `getX` and `getY` that define this representation. Finally, using your
-- | selectors and constructors, define a procedure `midpoint that takes a line
-- | segment as an argument and returns its midpoint (the point whose
-- | coordinates are the average of the coordinates of the endpoints). To try
-- | your procedures, you'll need a way to print points

    data Point = Point { getX :: Double, getY :: Double }
        deriving stock Eq

    instance Show Point where
        show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

    dotWith :: (Double -> Double -> Double) -> Point -> Point -> Point
    dotWith f p q = Point x y
      where
        x = (f `on` getX) p q
        y = (f `on` getY) p q

    data Segment = Segment { getStart :: Point, getEnd :: Point }
        deriving stock (Eq, Show)

    midpoint :: Segment -> Point
    midpoint (Segment start end) = dotWith average start end

    norm :: Segment -> Double
    norm (Segment p q) = sqrt . sumsquares $ dotWith (-) q p
      where
        sumsquares (Point x y) = square x + square y
