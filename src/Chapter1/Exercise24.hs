{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter1.Exercise24 (fastPrime, expmod) where
    import Chapter1.Exercise22 (makeSearchForPrimes)
    import Chapter1.Utilities (square, halveI)
    import Prelude hiding (exp)
    import System.Random (RandomGen, Random, randomRs, newStdGen)
-- | Modify the `timedPrimeTest` from exercise 1.22 to use `fastPrime` (the
-- | Fermat method), to test each of the 12 primes you found in that exercise.
-- | Since the Fermat method has Theta(log n) growth, how would you expect the
-- | time to test primes near 1,000,000 to compare with the time needed to test
-- | primes near 1,000? Do your data bear out? Can you explain the discrepancy
-- | you find?

    fastPrime :: (RandomGen g,  Integral a, Random a) => g -> Int -> a -> Bool
    fastPrime gen times n = and $ fmap fermat randoms
      where
          fermat a = a == expmod a n n
          randoms = take times (randomRs (1, n - 1) gen)

    expmod :: (Integral n) => n -> n -> n -> n
    expmod base exp m
        | exp == 0  = 1
        | even exp  = square (expmod base (halveI exp) m) `mod` m
        | otherwise = (base * expmod base (pred exp) m) `mod` m

    searchForPrimes :: (Integral n, Random n, Show n) => n -> IO ()
    searchForPrimes n = newStdGen >>= runSearch
      where
        runSearch gen = makeSearchForPrimes (fastPrime gen times) n
        times = 10 :: Int

-- >>> searchForPrimes 100000
-- 100003 *** 0s
-- 100019 *** 0s
-- 100043 *** 0.0010001s

-- >>> searchForPrimes 1000000
-- 1000003 *** 0.0010069s
-- 1000033 *** 0.001017s
-- 1000037 *** 0.0009994s

-- >>> searchForPrimes 10000000
-- 10000019 *** 0.0009982s
-- 10000079 *** 0.0009993s
-- 10000103 *** 0.0020044s

-- >>> searchForPrimes 100000000
-- 100000007 *** 0.0020144s
-- 100000037 *** 0.0020014s
-- 100000039 *** 0.0020034s
