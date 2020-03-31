module Chapter1.Exercise28 (isPrime) where
-- | One variant of the Fermat test that cannot be fooled is called the Miller-
-- | Rabin test (Miller 1976; Rabin 1980). This starts from an alternate form
-- | of Fermat's Little Theorem, which states that if `n` is a prime number and
-- | `a` is any positive integer less than `n`, then `a` raised to the (`n`-1)th
-- | power is congruent to 1 modulo `n`. To test the primality of a number `n`
-- | by the Miller-Rabin test, we pick a random number `a` < `n` and raise `a`
-- | to the (`n`-1)th power modulo `n` using the `expmod` procedure. When we
-- | perform the squaring step in `expmod`, however, we check to see if we have
-- | discovered a "non-trivial square root of 1 modulo `n`," that is, a number
-- | not equal to 1 or (`n`-1) whose square is equal to 1 modulo `n`. It is
-- | possible to prove that if such a non-trivial square root of 1 modulo `n`
-- | exists, then `n` is not prime. It is also possble to prove that if `n` is
-- | an odd number that is not prime, then, for at least half the numbers
-- | `a` < `n`, computing a^(n-1) in this way will reveal a non-trivial square
-- | root of 1 modulo `n`. (This is why the Miller-Rabin test cannot be fooled).
-- | Modify the `expmod` procedure to signal if it discovers a non-trivial
-- | square root of 1 modulo n, and use this to implement the Miller-Rabin test
-- | with a procedure analogous to `fermat`. Check your procedure by testing
-- | various primes and non-primes. Hint: One convenient way to make `expmod`
-- | signal is to have it return 0.
    import Chapter1.Utilities (halveI, square)
    import Prelude hiding (exp)
    import System.Random (Random, RandomGen, randomRs, newStdGen)

    fastPrime :: (RandomGen g,  Integral a, Random a) => g -> a -> Bool
    fastPrime gen n = and $ fmap millerRabin randoms
      where
          millerRabin a = 1 == expmod a (n - 1) n
          randoms = take times (randomRs (1, n - 1) gen)
          times = (fromIntegral $ halveI n) :: Int

    expmod :: (Integral n) => n -> n -> n -> n
    expmod base exp m = let x = go exp in if isNonTrivial x then 0 else x
      where
        go n | n == 0    = 1
             | even n    = square (expmod base (halveI n) m) `mod` m
             | otherwise = (base * expmod base (pred n) m) `mod` m
        isNonTrivial 1 = False
        isNonTrivial x = odd m && x /= (m - 1) && square x `mod` m == 1

    isPrime :: (Integral n, Random n) => n -> IO Bool
    isPrime n = newStdGen >>= \g -> return (fastPrime g n)
