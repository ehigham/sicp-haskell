module Chapter1.Exercise22 (timedPrimeTest) where
    import Chapter1.Exercise21 (smallestDivisor)
    import Control.Monad (when)
    import Data.Time.Clock
-- | Most Lisp implementations include a primative called `runtime` that returns
-- | an integer that specifies the amount of time the system has been running
-- | (measured, for example, in microseconds). The following `timedPrimeTest`,
-- | when called with an integer `n`, prints `n` and checks to see if `n` is
-- | prime. If `n` is prime, the procedure prints three asterisks followed by
-- | the amount of time used in performing the test.

    timedPrimeTest :: (Integral n, Show n) => n -> IO ()
    timedPrimeTest n = do
            putStr (show n)
            (prime, time) <- timeIt . return $ isPrime n
            when prime $ reportPrime time
            newLine
        where
            newLine = putStrLn ""
    
    isPrime :: (Integral n) => n -> Bool
    isPrime n = n == (smallestDivisor n)

    reportPrime :: (Show n) => n -> IO ()
    reportPrime time = putStrLn (" *** " ++ show time)

    timeIt :: IO a -> IO (a, NominalDiffTime)
    timeIt ma = do
            start <- getCurrentTime
            a <- ma
            finish <- getCurrentTime
            return (a, diffUTCTime finish start)

-- | Using this procedure, write a procedure `searchForPrimes` that checks the
-- | primality of consecutive odd integers in a specified range. Use your
-- | procedure t find the smallest prime numbers larher than 1,000; karger
-- | than 10,000; larger than 100,000; larger than 1,000,000. Note the time
-- | needed to test each prime. Since the testing algorithm has an order of
-- | growth of Theta(sqrt n), you should expect that the testing for primes
-- | around 10,000 should take about sqrt 10 times as long as testing for primes
-- | around 1,000. Do your timing data bear this out? How well do the data for
-- | 100,000 and 1,000,000 support the (sqrt n) prediction? Is your result
-- | compatible with the notion that programs on your machine run in time
-- | proportional to the numbre of steps required for the computation?

    searchForPrimes :: (Integral n) => n -> IO[(n, NominalDiffTime)]
    searchForPrimes n = sequence $ fmap g primes
        where
            primes = [p | p <- [p0..], isPrime p]
            p0 = findFirstPrime n
            g x = do
                (_, time) <- timeIt $ return ( isPrime x )
                return (x, time)
    
    findFirstPrime :: (Integral n) => n -> n
    findFirstPrime n
        | n < 3     = 2
        | even n    = findFirstPrime (succ n)
        | isPrime n = n
        | otherwise = findFirstPrime (n + 2)