module Chapter1.Exercise6 (sqrt') where
    import Chapter1.Utilities (improve, square, halve)
    
    -- append `_` to diambiguate with Prelude.sqrt
    sqrt' :: (Real x, Fractional x) => x -> x
    sqrt' x = sqrtIter (halve x) x

    sqrtIter :: (Real x, Fractional x) => x -> x -> x
    sqrtIter guess x
        | goodEnough guess x = guess
        | otherwise = sqrtIter (improve guess x) x

    goodEnough :: (Real x, Fractional x) => x -> x -> Bool
    goodEnough guess x = abs (square guess - x) < threshold
        where threshold = 0.001


-- | Alyssa P. Hacker doesn't see why `if` needs to be provided as a special
-- | "Why can't I just define it as an ordinary procedure in terms of `cond`?"
-- | she asks. Alyssa's friend Eva Lu Ator claims that this can indeed be done,
-- | and she defines a new version of if:

    newIf :: Bool -> a -> a -> a
    newIf predicate consequent alternative
        | predicate = consequent
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
-- | - Scheme uses applicative order evaluation and so the program will loop
-- | indefinitely while the arguments to newIf are evaluated.
-- | - Haskell uses lazy evaluation meaning expressions are evaluated when they
-- | are needed. Thus, newIf will work!