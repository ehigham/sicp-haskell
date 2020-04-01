{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter1.Exercise37 (calculatePhi) where
-- | a. An infinite "continued fraction" is an expression of the form
-- @
--     f = N1 / (D1 + N2 / (D2 + N3 / (D3 + ...))))
-- @
-- | As an example, one can show that the infinite continued fraction expansion
-- | Ni and Di all equal to 1 produces 1/phi, where phi is the golden ratio (de-
-- | scribed in section 1.2.2). One way to approximate an infinite continued
-- | fraction is to truncate the expansion after a given number of terms. Such
-- | a truncation - a so-called k-term finite continued fraction - has the form:
-- @
--     f = N1 / (D1 + N2 / ( ... + Nk / Dk ...)))
-- @
-- | Suppose that n and d are procedures of one argument (the term index `i`)
-- | that return the Ni and Di of the terms of the fraction. Define a procedure
-- | `contFrac` such that evaluating `contFrac n d k` computes the value of the
-- | k-term finite continued fraction. Check your procedure by approximating
-- | 1/phi using
-- @
--    contFrac (const 1) (const 1) k
-- @
-- | for successive values of k.
-- | How large must you make k in order to get an approximation that is accurate
-- | to 4 decimal places?

    contFrac :: (Int -> Double) -> (Int -> Double) -> Int -> Double
    contFrac ns ds = go 0
      where
        go acc k | k < 1     = acc
                 | otherwise = go (ns k / (ds k + acc)) (k - 1)

    calculatePhi :: Int -> Double
    calculatePhi = (1.0 /) . contFrac (const 1) (const 1)

-- >>> calculatePhi 4
-- 1.6666666666666665
--
-- >>> calculatePhi 8
-- 1.619047619047619
--
-- >>> calculatePhi 12
-- 1.6180555555555558
--
-- >>> calculatePhi 14
-- 1.6180371352785146
-- | k = 12

-- | If your `contFrac` procedure generates a recursive process, write one
-- | that generates an iterative process. If it generates an iterative process,
-- | write one that generates a recursive process

    contFrac' :: (Int -> Double) -> (Int -> Double) -> Int -> Double
    contFrac' ns ds k | k < 1     = 0
                      | otherwise = ns k / (ds k + contFrac ns ds (k - 1))