module Chapter2.Exercise27 (deepReverse) where

import Chapter2.Exercise24 (Tree (Node))

-- | Modify your `reverse` procedure of exercise 2.18 to produce a `deepReverse`
-- procedure that takes a `Tree` as argument and returns as its value the
-- `Tree` with its elements reversed and will all subtrees deep-reversed as
-- well.
deepReverse :: Tree a -> Tree a
deepReverse (Node xs) = Node $ foldl (flip ((:) . deepReverse)) [] xs
deepReverse leaf = leaf
