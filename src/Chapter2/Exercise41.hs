module Chapter2.Exercise41 (orderedTriples) where

-- | Write a procedure to find all the ordered triples of distinct positive
-- `i`, `j` and `k` less than or equal to a given integer `n` that sum to a
-- given integer `s` (assuming i <= j <= k <= n):
orderedTriples :: Integer -> Integer -> [(Integer, Integer, Integer)]
orderedTriples n s =
    [(i, j, k) | i <- [1..n], j <- [i..n], k <- [j..n], i + j + k == s]

