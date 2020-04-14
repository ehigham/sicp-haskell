{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter1.Exercise44 (smooth) where
    import Chapter1.Exercise43 (repeated)
-- | The idea of "smoothing" a function is an important conept in signal
-- | processing. If `f` is a function and `dx` is some small number, then the
-- | smoothed version of `f` is the function whose value at point `x` is the
-- | average of f(x - dx), f(x) and f(x + dx). Write a procedure `smooth` that
-- | takes as input a procedure that computes `f` and returns a procedure that
-- | computes the the smoothed `f`. It is sometimes valuable to repeatedly
-- | smooth a function (that is, smooth the smoothed function, and so on) to
-- | obtain the n-fold smoothed function. Show how to generate the n-fold
-- | smoothed function of any given function using smooth and repeated from
-- | exercise 1.43

    smooth :: (Fractional a) => (a -> a) -> (a -> a)
    smooth f x = mean . fmap (f . (x+)) $ [negate dx, 0, dx]
      where
        mean xs = sum xs / fromIntegral (length xs)
        dx :: (Fractional b) => b
        dx = 0.00001

    smoothn :: (Fractional a) => (a -> a) -> Int -> (a -> a)
    smoothn = repeated . smooth
