module Chapter1.Exercise22 (makeSearchForPrimes) where
    import Chapter1.Exercise21 (smallestDivisor)
    import Chapter1.Utilities (timeIt)
    import Control.Monad (when)

-- | Most Lisp implementations include a primative called `runtime` that returns
-- | an integer that specifies the amount of time the system has been running
-- | (measured, for example, in microseconds). The following `timedPrimeTest`,
-- | when called with an integer `n`, prints `n` and checks to see if `n` is
-- | prime. If `n` is prime, the procedure prints three asterisks followed by
-- | the amount of time used in performing the test.

-- | Modified definition for re-use with Exercise23:

    timedPrimeTest :: (Integral n, Show n) => (n -> Bool) -> n -> IO ()
    timedPrimeTest isPrime n = do
        putStr (show n)
        (prime, time) <- timeIt $ isPrime n
        when prime $ reportPrime time
      where
        newLine = putStrLn ""

    reportPrime :: (Show n) => n -> IO ()
    reportPrime time = putStrLn (" *** " ++ show time)

-- | Using this procedure, write a procedure `searchForPrimes` that checks the
-- | primality of consecutive odd integers in a specified range. Use your
-- | procedure to find the three smallest prime numbers larger than 1,000;
-- | larger than 10,000; larger than 100,000; larger than 1,000,000. Note the
-- | time needed to test each prime. Since the testing algorithm has an order of
-- | growth of Theta(sqrt n), you should expect that the testing for primes
-- | around 10,000 should take about sqrt 10 times as long as testing for primes
-- | around 1,000. Do your timing data bear this out? How well do the data for
-- | 100,000 and 1,000,000 support the (sqrt n) prediction? Is your result
-- | compatible with the notion that programs on your machine run in time
-- | proportional to the number of steps required for the computation?

    searchForPrimes :: (Integral n, Show n) => n -> IO ()
    searchForPrimes = makeSearchForPrimes isPrime

    makeSearchForPrimes :: (Integral n, Show n) => (n -> Bool) -> n -> IO ()
    makeSearchForPrimes isPrime n
        = sequence_ $ take 3 $ fmap (timedPrimeTest isPrime) primes
      where
        primes = [p | p <- [(from n)..], isPrime p]
        from n
            | isPrime n = n
            | even n    = from (n + 1)
            | otherwise = from (n + 2)

    isPrime :: (Integral n) => n -> Bool
    isPrime n = n == smallestDivisor n

-- | As of 2020, there's too much noise in the timings below ~100,000 so we'll
-- | start our test from here.

-- >>> searchForPrimes 100000
-- 100003 *** 0.000415891s
-- 100019 *** 0.00036075s
-- 100043 *** 0.00056723s

-- >>> searchForPrimes 1000000
-- 1000003 *** 0.00184107s
-- 1000033 *** 0.001849955s
-- 1000037 *** 0.001836786s

-- >>> searchForPrimes 10000000
-- 10000019 *** 0.005914216s
-- 10000079 *** 0.00426524s
-- 10000103 *** 0.00598021s

-- >>> searchForPrimes 100000000
-- 100000007 *** 0.016011287s
-- 100000037 *** 0.019222973s
-- 100000039 *** 0.013291527s

-- | The factor of growth between orders of magnitude is ~3 ~= (sqrt 10).
-- | We can confirm the notion that programs on your machine run in time
-- | proportional to the number of steps required for the computation.
