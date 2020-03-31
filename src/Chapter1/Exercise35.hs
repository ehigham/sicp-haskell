module Chapter1.Exercise35 (fixed, fixedM, phi) where
    import Control.Monad.Identity (runIdentity)
-- | Show that the golden ratio `phi` (section 1.2.2) is a fixed point of the
-- | transfromation \x -> 1 + 1/x, and use this fact to compute phi by means of
-- | the `fixedPoint` procedure

-- | Proof of `phi` being a fixed point (via quadratic formula, discard +-)
-- @
--     x   = 1 + 1/x
--     x^2 = x + 1
--     x = (1 + sqrt 5)/2
-- @

    fixedM :: (Monad m, Real a, Fractional a) => (a -> m a) -> a -> m a
    fixedM f a = do
        b <- f a
        if closeEnough b
            then return b
            else fixedM f b
      where
        closeEnough b = abs (a - b) < 0.00001

    fixed :: (Real a, Fractional a) => (a -> a) -> a -> a
    fixed f a = runIdentity $ fixedM (return . f) a

    phi :: Double
    phi = fixed (\x -> 1 + 1/x) 1.6
