module Chapter1.Utilities (goodEnough, sqrt_, improve) where

-- append `_` to diambiguate with Prelude.sqrt
    sqrt_ :: (Real x, Fractional x) => x -> x
    sqrt_ x = sqrtIter (half x) x

    sqrtIter :: (Real x, Fractional x) => x -> x -> x
    sqrtIter guess x
        | goodEnough guess x = guess
        | otherwise = sqrtIter (improve guess x) x

    goodEnough :: (Real x, Fractional x) => x -> x -> Bool
    goodEnough guess x = abs (square guess - x) < threshold
        where threshold = 0.001

    improve :: (Fractional x) => x -> x -> x
    improve guess x = average guess (x / guess)

    average :: (Fractional x) => x -> x -> x
    average x y = half $ x + y;

    half :: (Fractional x) => x -> x
    half x = x / 2.0

    square :: (Num x) => x -> x
    square x = x * x