module Chapter2.Exercise7 (
    Interval,
    mkInterval,
    mkCentreWidth,
    mkCentrePercent,
    lower,
    centre,
    upper,
    width,
    percent
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

-- | After debugging her program, Alyssa shows it to a potential user, who
-- | complains that her program solves the wrong problem. He wants a program
-- | that can deal with centre value and additive tolerance; for example, he
-- | wants to work with intervals such as 3.5 +/- 0.15 rather than [3.35, 3.65].
-- | Alyssa returns to her desk and fixes her problem by supplying an alternate
-- | constructor and alternate selectors:

    mkCentreWidth :: Double -> Double -> Interval
    mkCentreWidth c w = mkInterval (c - w) (c + w)

    width :: Interval -> Double
    width = abs . halve . liftM2 (-) upper lower

    centre :: Interval -> Double
    centre = liftM2 average lower upper

-- | Unfortunately, most of Alyssa's users are engineers. Real engineering
-- | situations usually involve measurements with only a small uncertainty,
-- | measured as the ratio of the width of the interval to the midpoint of the
-- | interval. Engineers usually specify percentage tolerances on the parameters
-- | of devices, as in the resistor specifications given earlier.

-- | Exercise 2.12
-- | Define a constructor `mkCentrePercent` that takes a centre and a percentage
-- | tolerance and produces the desired interval. You must also define a
-- | selector `percent` that produces the percentage tolerance for a given
-- | interval. The `centre` selector is the same as the one shown above.

    mkCentrePercent :: Double -> Double -> Interval
    mkCentrePercent = liftM2 (.) mkCentreWidth ((*) . (0.01 *))

    percent :: Interval -> Double
    percent = (* 100) . liftM2 (/) width centre
