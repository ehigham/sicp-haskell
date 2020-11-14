module Chapter1.Exercise20 (gcd) where
import Prelude hiding (gcd)
-- | The process that a procedure generates is of course dependent on the rules
-- used by the interpreter. As an example, consider the iterative `gcd`
-- procedure given below.
gcd :: (Integral n) => n -> n -> n
gcd a b = if b == 0 then a else gcd b (a `mod` b)

-- | Suppose we were to interpret this procedure using normal-order evaluation,
-- as dicussed in section 1.1.5 (The normal-order evaluation rule for `if` is
-- described in exercise 1.5). Using the substitution method (for normal-
-- order), illustrate the process generated in evaluating `gcd 206 40` and
-- indicate the `mod` operations that are actually performed.
--
-- >>> gcd 206 40
-- gcd 40 (206 `mod` 40)
-- gcd (206 `mod` 40) (40 `mod` (206 `mod` 40))
-- gcd (40 `mod` (206 `mod` 40)) ((206 `mod` 40) `mod` (40 `mod` (206 `mod` 40)))
-- gcd ((206 `mod` 40) `mod` (40 `mod` (206 `mod` 40)) ((40 `mod` (206 `mod` 40)) `mod` ((206 `mod` 40) `mod` (40 `mod` (206 `mod` 40))))
-- 2

-- | How many `mod` operations are actually performed in the normal-order
-- evaluation of `gcd 206 40?
-- 18 `mod`s`: - 14 on expansion (evaluating the predicate)
--             - 4 on reduction (evaluating the consequent).

-- | In the applicative order evaluation?
-- >>> gcd 206 40
-- gcd 40 6   -- (206 `mod` 40) evaluated
-- gcd 6 4    -- (40 `mod` 6) evaluated
-- gcd 4 2    -- (6 `mod` 4) evaluated
-- gcd 2 0    -- (4 `mod` 2) evaluated
-- 2
-- | 4.

-- | Interesting note: Haskell uses lazy evaluation, which binds a value to a
-- name only when it is required. Thus, the process generated in Haskell is:
-- >>> gcd 206 40
-- gcd 40 (206 `mod` 40)
-- gcd 6 (40 `mod` 6)   -- (206 `mod` 40) evaluated
-- gcd 4 (6 `mod` 4)    -- (40 `mod` 6) evaluated
-- gcd 2 (4 `mod` 2)    -- (6 `mod` 4) evaluated
-- 2                    -- (4 `mod` 2) evaluated
