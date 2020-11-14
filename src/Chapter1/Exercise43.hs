module Chapter1.Exercise43 (repeated) where
-- | If `f` is a numerical function and `n` is a positive integer, then we can
-- form the `n`th repeated application if f, which is defined to be the
-- function whose value at `x` is `(f.f. ... .f) x`. For example, if `f` is
-- the function (+1), then the `n`th repeated application of `f` is the
-- function (+n). If `f` is the operation of squaring a number, then the `n`th
-- repeated application of `f` is the function that raises its argument to the
-- `2^n`th power. Write a procedure that takes as inputs a procedure that
-- computes `f` and a positive integer `n` and returns the procedure that
-- computes the `n`th repeated application of `f`. Your procedure should be
-- able to be ised as follows:
-- >>> (repeated square 2) 5
-- 625
repeated :: (a -> a) -> Int -> (a -> a)
repeated f n = (!! n) . iterate f
