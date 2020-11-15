module Chapter2.Exercise40 (primeSumPairs, uniquePairs) where

import Chapter1.Exercise23 (isPrime)

-- | Define a procedure `uniquePairs` that, given an integer `n`, generates
-- the squence of pairs `(i, j)`,`1 <= j < i <= n`. Use `uniquePairs` to
-- simplify the defiinition pf `primeSumPairs` given below.
uniquePairs :: Integer -> [(Integer, Integer)]
uniquePairs n = [(i, j) | i <- [1..n], j <-[1..(pred i)]]

primeSumPairs :: Integer -> [(Integer, Integer, Integer)]
primeSumPairs = filter (isPrime . third) . map makePairSum . uniquePairs
  where
    makePairSum (i, j) = (i, j, i+j)
    third (_, _, x) = x

-- >>> primeSumPairs 6
-- [(2,1,3),(3,2,5),(4,1,5),(4,3,7),(5,2,7),(6,1,7),(6,5,11)]
