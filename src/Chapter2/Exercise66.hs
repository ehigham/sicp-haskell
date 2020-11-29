module Chapter2.Exercise66 (lookup, Known) where

import Prelude hiding (lookup)
import Chapter2.Exercise63 (Tree (Empty, Leaf, Node))
-- Implement the `lookup` procedure for the case where the set of records is
-- structured as a binary tree, ordered by the numerical values of the keys.

class Known s where
    key :: Ord a => s -> a

lookup :: (Ord a, Known b) => a -> Tree b -> Maybe b
lookup _ Empty        = Nothing
lookup a (Leaf x)     | a == key x = Just x
                      | otherwise  = Nothing
lookup a (Node l x r) | a == key x = Just x
                      | a < key x  = lookup a l
                      | otherwise  = lookup a r
