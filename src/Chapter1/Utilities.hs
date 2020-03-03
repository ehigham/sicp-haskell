module Chapter1.Utilities (improve, average, halve, square, cube) where

    improve :: (Fractional x) => x -> x -> x
    improve guess x = average guess (x / guess)

    average :: (Fractional x) => x -> x -> x
    average = (halve .) . (+)

    halve :: (Fractional x) => x -> x
    halve = (* 0.5)

    square :: (Num x) => x -> x
    square x = x * x

    cube :: (Num x) => x -> x
    cube x = x * x * x