module Chapter1.Exercise45 (rootN) where
    import Chapter1.Exercise35 (fixedPoint)
    import Chapter1.Exercise36 (averageDamp)
    import Chapter1.Exercise43 (repeated)
    import Control.Monad (join)
-- | We saw in section 1.3.3 that attempting to compute square roots by naively
-- | finding the fixed-point of \y -> x / y does not converge and that this can
-- | be fixed by average damoing. The same method works for finding cube roots
-- | as fixed points of the average-damped \y -> x / y^2. Unfortunately, this
-- | method does not work for fourth roots - a simple average damp is not enough
-- | to make a fixed point search of \y -> x / y^3 converge. On the other hand,
-- | if we average damp twice (i.e. use the avergae damp of the average damp of
-- | \y -> x / y^3) the fixed point seach does converge. Do some experiments to
-- | determine how many average damps are required to compute nth roots as a
-- | fixed point search based upon repeated average damping of \y -> x / y^n-1.
-- | Use this to implement a simple procedure for computing nth roots using
-- | `fixedPoint`, `averageDamp` and the `repeated` procedures of exercise 1.43.
-- | Assume that any arithmnetic operations you need are available as primitives.

    log2 :: (Floating a) => a -> a
    log2 = logBase 2

    rootN :: (RealFloat a) => a -> a -> a
    rootN n = join $ fixedPoint . (averageDamp `repeated` floor (log2 n)) . f
      where
        f x y = x / y^(round n - 1 :: Int)
