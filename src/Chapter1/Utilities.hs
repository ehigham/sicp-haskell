module Chapter1.Utilities (improve, average, half, square) where

    improve :: (Fractional x) => x -> x -> x
    improve guess x = average guess (x / guess)

    average :: (Fractional x) => x -> x -> x
    average = (half .) . (+)

    half :: (Fractional x) => x -> x
    half = (/ 2.0)

    square :: (Num x) => x -> x
    square = (^ 2)

    cube :: (Num x) => x -> x
    cube = (^ 3)