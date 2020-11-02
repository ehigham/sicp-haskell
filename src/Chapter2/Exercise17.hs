module Chapter2.Exercise17 (lastPair) where
-- | Define a procedure `lastPair` that returns the list that contains only
-- | the last element of a given (nonempty) list
-- >>> lastPair [23, 72, 149, 34] :: [Int]
-- [34]

    lastPair :: [a] -> [a]
    lastPair = pure . foldr1 (const id)

