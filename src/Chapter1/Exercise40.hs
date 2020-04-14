module Chapter1.Exercise40 (newtonsMethod, cubic) where
    import Chapter1.Exercise35 (fixed)
    import Chapter1.Utilities (square, cube)
-- | Define a procedure `cubic` that can be used together with the
-- | `newtonsMethod` procedure in expressions of the form:
-- @
--      newtonsMethod (cubic a b c) 1
-- @
-- | to approximate zeros of the cubix x^3 + a.x^2 + b.x + c.

    derivative :: (Real a, Fractional a) => (a -> a) -> a -> a
    derivative f x = (f (x + dx) - f x) / dx
      where
        dx :: (Fractional b) => b
        dx = 0.00001

    newtonsMethod :: (Real a, Fractional a) => (a -> a) -> a -> a
    newtonsMethod = fixed . newtonTransform
      where
        newtonTransform f x = x - f x / derivative f x

    cubic :: (Num a) => a -> a -> a -> a -> a
    cubic a b c x = cube x + a * square x + b * x + c
