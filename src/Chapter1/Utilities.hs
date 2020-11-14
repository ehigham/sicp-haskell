module Chapter1.Utilities (
    double,
    improve,
    average,
    halve,
    halveI,
    square,
    cube,
    divides,
    timeIt
)
    where
import Control.Exception (evaluate)
import Data.Time.Clock

double :: (Num x) => x -> x
double x = x + x

improve :: (Fractional x) => x -> x -> x
improve guess x = average guess (x / guess)

average :: (Fractional x) => x -> x -> x
average = (halve .) . (+)

halve :: (Fractional x) => x -> x
halve = (* 0.5)

halveI :: (Integral x) => x -> x
halveI x = x `div` 2

square :: (Num x) => x -> x
square x = x * x

cube :: (Num x) => x -> x
cube x = x * x * x

{-# INLINABLE divides #-}
divides :: (Integral n) => n -> n -> Bool
divides x y = y `mod` x == 0

timeIt :: a -> IO (a, NominalDiffTime)
timeIt f = do
    start <- getCurrentTime
    a <- evaluate f
    finish <- getCurrentTime
    return (a, diffUTCTime finish start)
