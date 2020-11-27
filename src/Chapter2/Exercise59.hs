{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Chapter2.Exercise59 ( UnorderedSet(UnorderedSet) ) where

import Control.Applicative (Alternative)
import Chapter2.Set (Set(..))

newtype UnorderedSet a = UnorderedSet [a]
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

instance Set UnorderedSet where
    adjoin x s = if x `isElem` s then s else pure x <> s

    isElem a (UnorderedSet elems) = go elems
      where
        go []     = False
        go (x:xs) = a == x || go xs

    intersect a b = a >>= \x -> if x `isElem` b then pure x else mempty

    -- | Implement the `union` operation for the unordered list representation
    -- of sets.
    union = foldl (flip adjoin)

instance (Show a) => Show (UnorderedSet a) where
    show (UnorderedSet elems) = "{" ++ go elems ++ "}"
      where
        go [] = ""
        go xs = foldr1 (\a b -> a ++ "," ++ b) $ map show xs

instance (Ord a, Eq a) => Eq (UnorderedSet a) where
    a == b = and $ fmap (`isElem` a) b

