module Chapter1.Exercise29 (simpson, integral) where
-- | Simpson's rule is a more accurate method of numerical integration than the
-- | method shown on page 59. Using Simpson's rule, the integral of a function
-- | `f` between `a` and `b` is approximated by:
-- |
-- |  (h/3) . (y0 + 4.y1 + 2.y2 + 4.y3 + 2.y4 +...+ 2.y(n-2) + 4.y(n-1) + yn)
-- |
-- | where h = (b - a)/n, for some even integer n, and yk = f(a + kh).
-- | (Increasing n increases the accuracy of the approxiation). Define a
-- | procedure that takes as arguments `f`, `a`, `b` and `n` and returns the
-- | value of the integral, computed using Simpson's rule. Use your procedure to
-- | integrate `cube` between 0 and 1 (with n = 100 and n = 1000) and compare
-- | the results to those of the aforementioned `integral` procedure.
    import Chapter1.Utilities (halve, cube)

    simpson :: (Fractional a) => (a -> a) -> a -> a -> Int -> a
    simpson f a b n = (/3.0) . (h*) . sum $ zipWith (*) coeffs ys
      where
        h = (b - a) / fromIntegral n
        coeffs = 1 : 1 : cycle [4, 2]
        ys = take (n + 1) $ fmap yk (0:n:[1..])
        yk k = f $ a + h * fromIntegral k

    integral :: (Real a, Fractional a) => (a -> a) -> a -> a -> a -> a
    integral f a b dx = (*) dx . sum . fmap f $ xs
      where
        xs = takeWhile (<=b) $ xsFrom a
        xsFrom x = x + halve dx : xsFrom (x + dx)

-- | Simpson's rule is 4th order accurate, meaning that the error scales
-- | asymptotically with h^4, whereas the trapezoidal method is 2nd order
-- | accurate. For the same h, therefore, we'd expect `simpson` to achieve a
-- | better approximiation for `cube` than `integral`:
--
-- >>> cube x = x * x * x
-- >>> simpson cube 0 1 100
-- 0.25000000000000006
--
-- >>> simpson cube 0.0 1.0 1000
-- 0.25000000000000006
-- | ^ probably reaching limits of numerical precision here
