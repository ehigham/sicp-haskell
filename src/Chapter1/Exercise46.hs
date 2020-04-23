{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter1.Exercise46 (iterativeImprove) where
    import Chapter1.Utilities (improve, square)
    import Prelude hiding (sqrt)
-- | Several of the numerical methods described in this chapter are instances
-- | of an extremely general computational strategy known as `iterative
-- | improvement`. Iterative improvement stats that, to compute something, we
-- | start with an initial guess for the answer, test if the guess is good
-- | enough and otherwise improve the guess and continue the process using the
-- | improved guess as the new guess. Write a procedure `iterativeImprove` that
-- | takes two procedures as arguments: a method for telling whether a guess is
-- | good enough and a method for improving a guess. `iterativeImprove` should
-- | return as its value a procedure that takes a guess as argument and keeps
-- | improving the guess until it is good enough. Rewrite the `sqrt` procedure
-- | from section 1.1.7 and the `fixedPoint` procedure of section 1.3.3 in terms
-- | of `iterativeImprove`.

    iterativeImprove :: (a -> Bool) -> (a -> a) -> (a -> a)
    iterativeImprove test next = go
      where
        go x = if test x then x else go (next x)

    sqrt :: Double -> Double
    sqrt x = iterativeImprove goodEnough (`improve` x) x
      where
        goodEnough guess = abs (square guess - x) < 0.001

    fixedPoint :: (Double -> Double) -> (Double -> Double)
    fixedPoint f = iterativeImprove goodEnough f
       where
         goodEnough x = abs (x - f x) < 0.00001
