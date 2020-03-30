module Chapter1.Exercise30 (sum) where
    import Prelude hiding (sum)
-- | The `sum` procedure generates a linear recusion. The procedure can be
-- | written so that the sum is performed iteratively. Show how to do this by
-- | filling in the mising expressions in the following definition:

    sum :: (Num a, Ord a) => (a -> a) -> a -> (a -> a) -> a -> a
    sum term a next b = go a 0
      where
        go x acc | x > b = acc
                 | otherwise = go (next x) $ acc + term x
