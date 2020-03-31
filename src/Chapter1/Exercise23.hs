module Chapter1.Exercise23 (isPrime) where
    import Chapter1.Exercise22 (makeSearchForPrimes)
    import Chapter1.Utilities (square, divides)
-- | The `smallestDivisor` procedure shown at the start of this section does
-- | lots of needless testing: Afer it checks to see if the number is divisible
-- | by 2 there is no point in checking to see if it is divisible by any larger
-- | even numbers. This suggests that the values used for test-divisor should
-- | not be [2, 3, 4, 5, 6, ...], but rather [2, 3, 5, 7, 9, ...].
-- | To implement this change, define a procedure `next` that returns 3 if its
-- | input is equal to 2 and otherwise returns its input plus 2. Modify the
-- | smallestDivisor procedure to use `next guess` instead of `succ guess`.

    smallestDivisor :: (Integral n) => n -> n
    smallestDivisor = go 2
        where
            go test n
                | square test > n = n
                | test `divides` n  = test
                | otherwise         = go (next test) n
            next n = if n == 2 then 3 else n + 2

-- | With timed-prime-test incorporating this modified version of
-- | `smallestDivisor`, run the test for each of the 12 primes found in
-- | Exercise 1.22. Since this modification halves the number of test steps,
-- | you should expect it to run about twice as fast. Is this expectation
-- | confirmed? If not, what is the observed ratio of the speeds of the two
-- | algorithms, and how do you explain the fact that it is different from 2?

    searchForPrimes :: (Integral n, Show n) => n -> IO ()
    searchForPrimes = makeSearchForPrimes isPrime

    isPrime :: (Integral n) => n -> Bool
    isPrime n = n > 1 && n == smallestDivisor n

-- >>> searchForPrimes 100000
-- 100003 *** 0.000349968s
-- 100019 *** 0.000389157s
-- 100043 *** 0.000341747s

-- >>> searchForPrimes 1000000
-- 1000003 *** 0.001268654s
-- 1000033 *** 0.001133965s
-- 1000037 *** 0.001169275s

-- >>> searchForPrimes 10000000
-- 10000019 *** 0.002881546s
-- 10000079 *** 0.002978394s
-- 10000103 *** 0.003476213s

-- >>> searchForPrimes 100000000
-- 100000007 *** 0.010841799s
-- 100000037 *** 0.010658536s
-- 100000039 *** 0.010834093s

-- | This version of `isPrime` is about 1.5x faster than that used in Exercise
-- | 1.22. The overhead comes from the execution of an additional `if`
-- | statement.
