module Chapter1.Exercise4 (aPlusAbsB) where
-- | Observe that our model of evaluation allows for combinations whose
-- | operators are compond expressions, Use this observation to describe the
-- | behaviour of the following procedure:
--
aPlusAbsB :: (Ord a, Num a) => a -> a -> a
aPlusAbsB a b = (if b > 0 then (+) else (-)) a b

-- >>> aPlusAbsB 1 2
-- 3
-- >>> aPlusAbsB 1 (-2)
-- 1