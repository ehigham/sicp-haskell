module Chapter2.Exercise3 (
    Rectangular,
    area,
    perimeter,
    Rectangle (..),
    AltRectangle (..)
) where
    import Chapter1.Utilities (double)
    import Chapter2.Exercise2 (Segment, Point (getX, getY), norm)
    import Control.Monad (liftM2)
-- | Implement a representation for rectangles in a plan. (Hint: You may want
-- | to make use of Exercise 2.2). In terms of your constructors and selectors,
-- | create procedures that compure the perimeter and the area of a rectangle.
-- | Now implement a different representation for rectangles. Can you design
-- | your system with suitable abstraction barriers, so that the same perimeter
-- | and area procedures will work using either representation?

    class Rectangular r where
        height :: r -> Double
        width  :: r -> Double

    area :: (Rectangular r) => r -> Double
    area = liftM2 (*) height width

    perimeter :: (Rectangular r) => r -> Double
    perimeter = liftM2 (+) (double . height) (double . width)

    data Rectangle = Rectangle {
        base :: Segment,
        side :: Segment
    }
      deriving (Eq, Show)

    instance Rectangular Rectangle where
        height = norm . side
        width  = norm . base

    data AltRectangle = AltRectangle {
        bottomLeft :: Point,
        topRight   :: Point
    }
      deriving (Eq, Show)

    instance Rectangular AltRectangle where
        height r = getY (topRight r) - getY (bottomLeft r)
        width r  = getX (topRight r) - getX (bottomLeft r)
