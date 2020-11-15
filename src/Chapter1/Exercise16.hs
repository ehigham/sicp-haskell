module Chapter1.Exercise16 (fastExpt, fastExpt') where

import Chapter1.Utilities (square, halveI)
-- | Design a procedure that evolves an iterative exponentiation process that
-- uses successive squaring and uses a logarithmic number of steps, as does
-- `fastExpt`.
-- (Hint: Using the obersation (b^(n/2))^2 = (b^2)^(n/2), keep along with the
-- exponent `n` and the base `b`, an additional state variable `a` and define
-- the state transformation in such a way that the product a.b^n is unchanged
-- from state to state. At the beginning of the process `a` is taken to be 1
-- and the answer is given by the value of `a` at the end of the process. In
-- general, the technique of defining an "invariant quantity" that remains
-- unchanged from state to state is a powerful way to think about the design
-- of iterative algorithms).

fastExpt :: (Integral x) => x -> x -> x
fastExpt b n
    | n == 0 = 1
    | even n = square $ fastExpt b (halveI n)
    | otherwise = b * fastExpt b (n - 1)

fastExpt' :: (Integral x) => x -> x -> x
fastExpt' = go 1
    where
        go a b n
            | n == 0 = a
            | even n = go a (square b) (halveI n)
            | otherwise = go (a * b) b (n - 1)
