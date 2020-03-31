module Chapter1.Exercise35 (fixed, phi) where
-- | Show that the golden ratio `phi` (section 1.2.2) is a fixed point of the
-- | transfromation \x -> 1 + 1/x, and use this fact to compute phi by means of
-- | the `fixedPoint` procedure

-- | Proof of `phi` being a fixed point (via quadratic formula, discard +-)
-- @
--     x   = 1 + 1/x
--     x^2 = x + 1
--     x = (1 + sqrt 5)/2
-- @

    fixed :: (Real a, Fractional a) => (a -> a) -> a -> a
    fixed f y = go y (f y)
      where
          go a b | closeEnough a b = b
                 | otherwise       = go b (f b)
          closeEnough a b = abs (a - b) < 0.00001

    phi :: Double
    phi = fixed (\x -> 1 + 1/x) 1.6
