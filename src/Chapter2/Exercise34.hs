module Chapter2.Exercise34 (horner) where
-- | Evaluating a polynomial in `x` at a given value of `x` can be formulated
-- an accumulation. We can evaulate the polynomial
--
--   a_n * x^n + a_{n-1} * x ^{n-1} + ... + a_1 * x + a_0
--
-- using a well-known algorithm called Horner's rule, which structures the
-- computation as
--
--   (...(a_n*x + a_{n-1}) * x + ... + a_1) * x + a0
--
-- In other words, we start with `a_n`, mutiply by `x`, add `a_{n-1}`, multiply
-- by `x` and so on, until we reach `a_0`. Fill in the following template to
-- produce a procedure that evaluates a polynomial using Horner's rule. Assume
-- that the coefficients of the polynomial are arranged in a sequnce, from
-- `a_0` through `a_n`.

horner :: (Num a) => [a] -> a -> a
horner as x = foldr (\a terms -> a + x * terms) 0 as

-- | For example, to compute
-- @
--     f x = 1 + 3*x + 5*x^3 + x^5
-- @
-- at `x = 5`, you would write
-- >>> horner [1, 3, 0, 5, 0, 1] 2 :: Integer
-- 79
