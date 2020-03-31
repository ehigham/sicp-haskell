module Chapter1.Exercise33 (filteredAccumulate) where
    import Chapter1.Exercise20 (gcd)
    import Chapter1.Exercise23 (isPrime)
    import Chapter1.Utilities (square) 
    import Prelude hiding (gcd)
-- | You can obtain an even more general version of `accumulate` (exercise 1.32)
-- | by introducing the notion og a `filter` on the terms to be combined. That
-- | is, combine only those terms derived from the values in the range that
-- | satisify a specified condition. The resulting `filteredAccumulate`
-- | abstraction takes the same arguments as `accumulate`, together with an
-- | additional predicate of one argument that specifies the filter. Write
-- | `filteredAccumulate` as a procedure.

    type Combiner a = a -> a -> a
    type Filter a = a -> Bool

    filteredAccumulate :: (Ord a) => Combiner b -> b -> Filter a -> (a -> b) -> a -> (a -> a) -> a -> b
    filteredAccumulate combine state filter term a next b = go state a
      where
        go acc x | x > b     = acc
                 | otherwise = go (maybeCombine acc x) (next x)
        maybeCombine acc x = if filter x then combine acc (term x) else acc

-- | Show how to express the following using `filteredAccumulate`:
-- | a. the sum of the squares of the prime numbers in the interval a to b
-- | (Assume you have a `isPrime` predicate already written)

    sumSquarePrimes :: (Integral a) => a -> a -> a
    sumSquarePrimes a = filteredAccumulate (+) 0 isPrime square a (+1)

-- >>> sumSquarePrimes 1 5
-- 38
--
-- >>> sumSquarePrimes 1 1
-- 0

-- | b. the product of all the positive integers less than n that are relatively
-- | prime to n (i.e. all positive integers a < n such that GCD(i, n) = 1)

    relativelyPrimeProd :: (Integral a) => a -> a -> a
    relativelyPrimeProd a b = filteredAccumulate (*) 1 isRelativePrime id a (+1) b
      where
          isRelativePrime x = 1 == gcd x b

-- >>> relativelyPrimeProd 1 5
-- 24
