module Chapter1.Exercise6 where
    import Chapter1.Utilities (goodEnough, improve)
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

-- | Delighted, Alyssa uses `newIf` to rewrtie the square-root program:

    sqrtIter :: (Real x, Fractional x) => x -> x -> x
    sqrtIter guess x = 
        newIf (goodEnough guess x) guess (sqrtIter (improve guess x) x)

-- | What happens when Alyssa attempts to use this to compute square roots?
-- | The program will loop indefinitely while it tries to call `sqrtIter` on
-- | the improved guess. The built-in `if` evaluates the consequent and
-- | alternative as that branch is taken.