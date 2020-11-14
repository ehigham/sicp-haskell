module Chapter2.Exercise28 (fringe) where

import Chapter2.Exercise24 (Tree)
import Data.Foldable (toList)

-- | Write a procedure fringe that takes as argument a `Tree` as returns a
-- `List` whose elements are all the leaves of the tree arranged in left-
-- to-right order.
fringe :: Tree a -> [a]
fringe = toList
