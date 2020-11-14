module Chapter1.Exercise3 (sumLargestSquares) where
import Chapter1.Utilities (square)
import Data.List (sortBy)

-- | Define a procedure that takes three numbers as arugments and returns the
-- sum of the squares of the two larger numbers.
sumLargestSquares :: (Num n, Ord n) => n -> n -> n -> n
sumLargestSquares x y z =
    sum . map square . take 2 . sortBy (flip compare) $ [x, y, z]
