{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Chapter2.Exercise60 ( Bag (Bag) ) where

import Control.Applicative (Alternative)
import Chapter2.Set (Set(..))

-- | We specified that a set would be represented as a list with no duplicates.
-- now suppose we allow duplicates. Fir instance, the set {1, 2, 3} could be
-- represented as the list [2, 3, 2, 1, 3, 2, 2]. Implement `Set` for this
-- data type.

newtype Bag a = Bag [a]
  deriving stock    (
                        Foldable,
                        Functor,
                        Traversable
                    )
  deriving newtype  (
                        Applicative,
                        Alternative,
                        Monad,
                        Monoid,
                        Semigroup
                    )

instance Set Bag where
    adjoin = mappend . pure
    isElem a (Bag elems) = a `elem` elems
    intersect a b = a >>= \x -> if x `isElem` b then pure x else mempty
    union = mappend

instance (Show a) => Show (Bag a) where
    show (Bag elems) = "{" ++ go elems ++ "}"
      where
        go [] = ""
        go xs = foldr1 (\a b -> a ++ "," ++ b) $ map show xs

instance (Ord a, Eq a) => Eq (Bag a) where
    a == b = and $ fmap (`isElem` a) b

-- | How does the efficiency of each compare with the corresponding procedure
-- for the non-duplicate representation? Are there applications for which you
-- would use this representation in preference to the non-duplicate one?
--
--              ┌───────────┬──────────────┬──────────────┐
--              │  Method   │ UnorderedSet │     Bag      │
--              ├───────────┼──────────────┼──────────────┤
--              │  adjoin   │      N       │      1       │
--              ├───────────┼──────────────┼──────────────┤
--              │  isElem   │      N       │      N       │
--              ├───────────┼──────────────┼──────────────┤
--              │ intersect │     N^2      │     N^2      │
--              ├───────────┼──────────────┼──────────────┤
--              │   union   │     N^2      │      N       │
--              └───────────┴──────────────┴──────────────┘
--      Time complexities of `Set` operations for the two representations
