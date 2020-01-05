module Chapter1.Exercise3 (sumLargestSquares) where
-- | Define a procedure that takes three numbers as arugments and returns the
-- | sum of the squares of the two larger numbers.
    import Data.List (sortBy)

    square_ :: (Num n) => n -> n
    square_ x = x * x
    
    sumLargestSquares :: (Num n, Ord n) => n -> n -> n -> n
    sumLargestSquares x y z = 
        sum . map (square_) . take 2 . sortBy (flip compare) $ [x, y, z]
