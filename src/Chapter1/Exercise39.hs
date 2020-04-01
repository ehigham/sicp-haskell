module Chapter1.Exercise39 (tanCF) where
    import Chapter1.Exercise37 (contFrac)
    import Chapter1.Utilities (square)
-- | A continued fraction representaiton of the tangent function was published
-- | in 1770 by the german mathematician J. H. Lambert:
-- @
--      tan x = x /(1 - (x^2 /(3 - (x^2 /(5 - (...))))))
-- @
-- | where `x` is in radians. Define a procedure `tanCF x k` that computes an
-- | approximation to the tangent function based on Lambert's formula, where `k`
-- | specifies the number of terms to compute, as in exercise 1.37.

    tanCF :: Double -> Int -> Double
    tanCF x = negate . contFrac (negate . xs) ds
      where
        xs k = if k == 1 then x else square x
        ds k = fromIntegral $ 2 * k - 1

-- >>> tanCF (pi/4) 10
-- 1.0
