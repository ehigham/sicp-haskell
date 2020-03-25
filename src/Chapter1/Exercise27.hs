module Chapter1.Exercise27 (isCarmichael) where
    import Chapter1.Exercise23 (isPrime)
    import Chapter1.Exercise24 (expmod)
    import Control.Monad (liftM2)
-- | Demonstrate that the Carmichael numbers listed in footnote 47 really do
-- | fool the Fermat test. That is, write a procedure that takes an integer `n`
-- | and tests whether `a^n` is congruent to `a mod n` for every `a < n` and
-- | try your procedure on the given Carmichael numbers

    isCarmichael :: (Integral n) => n -> Bool
    isCarmichael = liftM2 (&&) passesFermat (not . isPrime)

    passesFermat :: (Integral n) => n -> Bool
    passesFermat n = and $ fmap fermat [1..(n - 1)]
      where
        fermat a = a == expmod a n n

-- | See TExercise27 for test