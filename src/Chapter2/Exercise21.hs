{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter2.Exercise21 () where
    import Chapter1.Utilities (square)
-- | The procedure `squareList` takes a list of numbers as arguments and returns
-- | a list of the squares of those numbers
--
-- >>> squareList [1, 2, 3, 4]
-- [1, 4, 9, 16]
--
-- | Here are two different definitions of `squareList`. Complete both of them
-- | by filling in the missing expressions:

    squareList :: (Num a) => [a] -> [a]
    squareList [] = []
    squareList (x:xs) = (square x):(squareList xs)

    squareList' :: (Num a) => [a] -> [a]
    squareList' = map square
