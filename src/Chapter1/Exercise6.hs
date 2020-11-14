{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter1.Exercise6 (sqrt') where
import Chapter1.Utilities (improve, square, halve)

-- append `'` to diambiguate with Prelude.sqrt
sqrt' :: (Real x, Fractional x) => x -> x
sqrt' x = go (halve x)
  where
    go guess | goodEnough guess = guess
             | otherwise        = go (improve guess x)
    goodEnough guess = abs (square guess - x) < 0.001

-- | Alyssa P. Hacker doesn't see why `if` needs to be provided as a special
-- "Why can't I just define it as an ordinary procedure in terms of `cond`?"
-- she asks. Alyssa's friend Eva Lu Ator claims that this can indeed be done,
-- and she defines a new version of if:
newIf :: Bool -> a -> a -> a
newIf predicate consequent alternative | predicate = consequent
                                       | otherwise = alternative

-- | Eva demonstrates the program for Alyssa:
-- >>> newIf ((==) 2 3) 0 5
-- 5
-- >>> newIf ((==) 1 1) 0 5
-- 0

-- | Delighted, Alyssa uses `newIf` to rewrite the square-root program:
-- @
--    sqrtIter :: (Real x, Fractional x) => x -> x -> x
--    sqrtIter guess x =
--        newIf (goodEnough guess x) guess (sqrtIter (improve guess x) x)
-- @

-- | What happens when Alyssa attempts to use this to compute square roots?
-- [Scheme] Uses applicative order evaluation and so the program will loop
-- indefinitely while the arguments to newIf are evaluated.
--
-- [Haskell] Uses lazy evaluation meaning expressions are evaluated when they
-- are needed. Thus, newIf will work!
