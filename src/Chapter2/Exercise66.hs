{-# OPTIONS_GHC -Wno-unsafe #-}

module Chapter2.Exercise66 (lookup, HasKey) where

import Prelude hiding (lookup)
import Chapter2.Exercise63 (Tree (Empty, Node))
-- Implement the `lookup` procedure for the case where the set of records is
-- structured as a binary tree, ordered by the numerical values of the keys.

class HasKey s where
    key :: Ord a => s -> a

lookup :: (Ord a, HasKey b) => a -> Tree b -> Maybe b
lookup _ Empty        = Nothing
lookup a (Node l x r) | a == key x = Just x
                      | a < key x  = lookup a l
                      | otherwise  = lookup a r
