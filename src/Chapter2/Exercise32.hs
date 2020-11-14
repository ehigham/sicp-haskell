module Chapter2.Exercise32 (subsets) where
-- | We can represent a set as a list of distinct elements, and we can represent
-- the set of all subsets of the set as a list of lists. For example, if the
-- set is [1, 2, 3], then the set of all subsets is
-- @
-- [[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]].
-- @
-- Complete the following definition of a procedire that generates the set of
-- subsets of a set and give a clear explanation of why it works.

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = rest ++ map (x:) rest
  where
    rest = subsets xs

-- | This works by computing the subsets of the rest of the list. To that set,
-- it appends the same set with each subset augmented with the first of list.
