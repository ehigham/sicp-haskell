module Chapter1.Exercise41 (double) where
    import Control.Monad (join)
-- | Define a procedure `double` that takes a procedure of one argument as an
-- | argument and returns a procedure that applies the original procedure twice.
-- | For example, if `inc` is a procedure that adds 1 to its argument, then
-- | `double inc` should be a procedure that adds two. What is the value
-- | returned by (double (double double)) inc 5?

    double :: (a -> a) -> (a -> a)
    double = join (.)

-- >>> let inc = (+1) in (double (double double)) inc (5 :: Int)
-- 21