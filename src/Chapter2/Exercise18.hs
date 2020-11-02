module Chapter2.Exercise18 (reverse') where
-- | Define a procedure `reverse` that takes a list as argument as returns a
-- | list of the same elements in reverse order.
    reverse' :: [a] -> [a]
    reverse' = foldl (flip (:)) []
