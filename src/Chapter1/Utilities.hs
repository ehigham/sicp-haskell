module Chapter1.Utilities (improve, average, half, square) where

    improve :: (Fractional x) => x -> x -> x
    improve guess x = average guess (x / guess)

    average :: (Fractional x) => x -> x -> x
    average x y = half $ x + y;

    half :: (Fractional x) => x -> x
    half x = x / 2.0

    square :: (Num x) => x -> x
    square x = x * x