module Chapter1.Exercise21 (smallestDivisor) where
    import Chapter1.Utilities (square, divides)
-- | Use the `smallestDivisor` procedure to find the smallest divisor of each of
-- | the following numbers: 199, 1999, 19999.

    smallestDivisor :: (Integral n) => n -> n
    smallestDivisor = go 2
      where
        go test n | square test > n   = n
                  | test `divides` n  = test
                  | otherwise         = go (succ test) n

-- >>> smallestDivisor 199
-- 199
-- >>> smallestDivisor 1999
-- 1999
-- >>> smallestDivisor 19999
-- 7
