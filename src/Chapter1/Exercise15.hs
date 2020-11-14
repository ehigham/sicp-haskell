module Chapter1.Exercise15 (sine, p) where
import Chapter1.Utilities (cube)

-- | The sine of an angle (specified in radians) can be computed by making use
-- of the approximation that sin(x) ~ x if x is sufficiently small, and the
-- trigonometric identity:
--
--     sin(x) = 3.sin(x/3) - 4.sin^3(x/3)
--
-- to reduce the size of the argument of sin. (For purposes of this exercise
-- an angle is considered "sufficiently small" if its magnitude is not greater
-- than 0.1 radians). These ideas are incorporated in the following
-- procedures:
sine :: (Real x, Fractional x) => x -> x
sine x = if sufficentlySmall x then x else p . sine . (/ 3) $ x

sufficentlySmall :: (Real x, Fractional x) => x -> Bool
sufficentlySmall = (<= 0.1) . abs

p :: (Real x) => x -> x
p x = 3 * x - 4 * cube x

-- | How many times is the procedure p applied when `sine 12.15` is
-- evaluated?
-- >>> sine 12.15
-- p . sine $ 4.05
-- p . p . sine $ 1.35
-- p . p . p . sine $ 0.45
-- p . p . p . p . sine $ 0.15
-- p . p . p . p . p $ 0.05
--
-- | `p` is evaluated 5 times.
