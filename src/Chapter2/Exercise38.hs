module Chapter2.Exercise38 (foldLeft, foldRight) where
-- | The `accumulate` procedure is also known as `foldRight` because it
-- combines the first element of the sequence with the result of combining all
-- the elements to the right. There is also `foldLeft` which is similar to
-- `foldRight` except that it combines elements working in the oposite
-- direction

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft f = go
  where
    go res []     = res
    go res (x:xs) = go (f res x) xs

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f = go
  where
    go res []     = res
    go res (x:xs) = f x (go res xs)


-- | What are the values of
-- >>> foldRight (/) 1 [1..3] :: Double
-- 1.5
--
-- >>> foldLeft (/) 1 [1..3] :: Double
-- 0.16666666666666666
--
-- >>> foldRight (:) [] [1..3] :: [Int]
-- [1,2,3]
--
-- >>> foldLeft (flip (:)) [] [1..3] :: [Int]
-- [3,2,1]

-- | Give a property that `f` should satisfy to guarantee that `foldRight` and
-- `foldLeft` will produce the same values for any sequence.
--
-- Associativity
