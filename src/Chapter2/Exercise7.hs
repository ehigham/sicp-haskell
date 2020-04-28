module Chapter2.Exercise7 (
    Interval,
    mkInterval,
    lower,
    centre,
    upper,
    width
) where
    import Chapter1.Utilities (average, halve)
    import Chapter2.Exercise4 (cons, car, cdr, Pair)
    import Control.Monad (liftM2, join)
    import Data.Function (on)
-- | Alyssa P. Hacker is designing a system to help people solve engineering
-- | problems. One feature she wants to preovide in her system is the ability
-- | to maniplulate inexact quantities (such as measured parameters of physical
-- | devices) with known precision, so that wben computations are done with
-- | such approximate quantities the results will be numbers of knwon precision.
--
-- | Electrical engineers will be using Alyssa's system to compute electrical
-- | quantities. It is sometimes necessary to compute the value of a parallel
-- | equivalent resistance `Rp` of two resistors `R1` and `R2` using the formula
-- @
--      Rp = recip $ recip R1 + recip R2
-- @
-- | Resistance values are usually known ip to some tolerance guaranteed by the
-- | manufacturer of the resistor. For example, if you buy a resistor labelled
-- | "6.8 ohms +/- 10%" you can only be sure that the reistor has a resistance
-- | between 6.8-0.68 = 6.12 and 6.8+0.68 = 7.48 ohms. Thus, if you have a 6.8
-- | ohm 10% resistor in parallel with a 4.7 ohm 5% resistor, the resistance of
-- | the combination can range from about 2.58 ohms (if the two resistors are
-- | at the lower bound) to about 2.97 ohms (if the two resistors are at the
-- | upper bounds).
--
-- | Alyssa's idea is to implement "interval arithmetic" as a set of arithmetic
-- | operations for combining "intervals" (objects that represent the range of
-- | possible values of an inexact quantity). The result of adding, subtracting,
-- | multiplying or dividing two intervals is itself an interval representing
-- | the range of the result.
--
-- | Alyssa postulates the existence of an abstract object called an "Interval"
-- | that has two endpoints: a lower bound and an upper bound. She also presumes
-- | that, given the endpoints of an interval, she can construct the interval
-- | using the data constructor `mkInterval` Alyssa first writes a procedure
-- | for adding two intervals. She reasons that the minimum value of the sum
-- | could be the sum of the two lower bounds and the maximum value it could be
-- | is the sum of the two upper bounds.

    instance Num Interval where
        a + b = mkInterval (lower a + lower b) (upper a + upper b)

-- | Alyssa also works out the product of the two intervals by finding the
-- | minimum and maximum of the products of the bounds and using them as the
-- | bounds of the resulting interval. (`minimum` and `maximum` are primitives
-- | that find the minimum and maximum of any number of arguments).
-- | EDIT: Exercise 2.11
        (*) = multInterval

-- | Exercise 2.8
-- | Using reasoning analogous to Alyssa's, describe hwo the difference of two
-- | intervals may be computed. Define a corresponding subtraction procedure (-)
        a - b = mkInterval (lower a - upper b) (upper a - lower b)

        abs a = mkInterval x y
          where
            x = abs (centre a) - width a
            y = abs (centre a) + width a

        signum = (join mkInterval) . signum . centre

        negate = liftM2 mkInterval (negate . upper) (negate . lower)

        fromInteger = (join mkInterval) . fromInteger

-- | Exercise 2.11
-- | In passing, Ben also cryptically commetns: "By testing the signs of the end
-- | points of the intervals, it is possible to break up `multInterval` into 9
-- | cases, only one requires more than two operations." Rewrite this procedure
-- | using Ben's suggestion.

    multInterval :: Interval -> Interval -> Interval
    multInterval a b = case (sign a, sign b) of
        (Positive, Positive) -> mkInterval (lower a * lower b) (upper a * upper b)
        (Positive, Spanning) -> mkInterval (upper a * lower b) (upper a * upper b)
        (Positive, Negative) -> mkInterval (upper a * lower b) (lower a * upper b)
        (Spanning, Positive) -> mkInterval (lower a * upper b) (upper a * upper b)
        (Spanning, Spanning) -> mkInterval (min (lower a * upper b) (upper a * lower b))
                                           (max (upper a * upper b) (lower a * lower b))
        (Spanning, Negative) -> mkInterval (upper a * lower b) (lower a * lower b)
        (Negative, Positive) -> mkInterval (lower a * upper b) (upper a * lower b)
        (Negative, Spanning) -> mkInterval (lower a * upper b) (lower a * lower b)
        (Negative, Negative) -> mkInterval (upper a * upper b) (lower a * lower b)

    data Sign = Positive | Negative | Spanning

    sign :: Interval -> Sign
    sign a = case (lower a `compare` 0, upper a `compare` 0) of
        (GT, GT) -> Positive
        (LT, LT) -> Negative
        _        -> Spanning


-- | Two divide two intervals, Alyssa multiplies the first by the reciprocal of
-- | the second. Note that the bounds of the reciprocal are the reciprocal of
-- | the upper bound and the reciprocal of the lower bound, in that order.

    instance Fractional Interval where
        a / b = a * recip b

-- | Exercise 2.10
-- | Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder
-- | and comments that it is not clear what it means to divide an interval that
-- | spans zero. Modify Alyssa's code to check for this condition and signal an
-- | error if it occurs.

        recip a = if spansZero a
            then error "interval division spans zero"
            else (mkInterval `on` recip) (upper a) (lower a)
          where
            spansZero x = lower x * upper x < 0

        fromRational = (join mkInterval) . fromRational

    instance Show Interval where
        show x = "[" ++ show (lower x) ++ ", " ++ show (upper x) ++ "]"

    instance Eq Interval where
        a == b = lower a == lower b && upper a == upper b

-- | Exercise 2.7
-- | Alyssa's program is incomplete because she has not specified the
-- | implementation of the interval abstraction. Here is a definition of the
-- | interval constructor:

    newtype Interval = Interval (Pair Double Double)

    mkInterval :: Double -> Double -> Interval
    mkInterval = (Interval .) . cons

-- | Define selectors `upper` and `lower` to complete the implementation.

    lower :: Interval -> Double
    lower (Interval a) = car a

    upper :: Interval -> Double
    upper (Interval a) = cdr a

-- | Exercise 2.9
-- | The `width` of an interval is half the difference between its upper and
-- | lower bounds. The width is a measure of the uncertainty of the number
-- | specified by the interval.
    width :: Interval -> Double
    width = abs . halve . liftM2 (-) upper lower

-- | For some arithmetic operations, the width of the result of combining two
-- | intervals is a function only of the argument intervals, whereas for others
-- | the width of the computation is not a function of the widths of the
-- | argument intervals. Show that the width of the sum (or difference) of two
-- | intervals is a function only of the widths of the intervals being added
-- | (or subtracted).
--
-- | For (+)
-- @
--      width $ a + b
--      width $ mkInterval (lower a + lower b) (upper a + upper b)
--      abs . halve $ (upper a + upper b) - (lower a + lower b)
--      abs . halve $ (upper a - lower a) + (upper b + lower b)
--      abs . halve $ 2 * width a + 2 * width b
--      -- only a function of width!
-- @
--
-- | Give examples to show that this is not true of multiplication and division.
-- | If multiplication and division were only functions of interval width, then
-- | one would expect multiplying different intervals of the saem width would
-- | yield the same result. For example, consider multiplying width 5 intervals
-- | with width 1:
-- |  [0, 10] * [0, 2]  = [0, 20]   (width = 10)
-- |  [-5, 5] * [-1, 1] = [-5, 5]   (width = 5)

    centre :: Interval -> Double
    centre = liftM2 average lower upper


    -- mult :: Interval -> Interval -> Interval
    -- mult a b = mkInterval (minimum ps) (maximum ps)
    --   where
    --     ps = [p1, p2, p3, p4]
    --     p1 = lower a * lower b
    --     p2 = lower a * upper b
    --     p3 = upper a * lower b
    --     p4 = upper a * upper b