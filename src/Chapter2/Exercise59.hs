{-# LANGUAGE DerivingVia #-}

module Chapter2.Exercise59 (UnorderedList) where

import Data.Foldable (toList)
import Data.Function (on)
import Chapter2.Set (Set(..))

newtype UnorderedList a = UnorderedList [a]
  deriving Foldable via []

instance Set UnorderedList where
    adjoin x s = if x `isElem` s then s else UnorderedList (x : toList s)

    isElem a (UnorderedList elems) = go elems
      where
        go []     = False
        go (x:xs) = a == x || go xs

    fromList = foldr adjoin mempty

    intersect a b = foldr (adjoinWhen (`isElem` a)) mempty b
      where
        adjoinWhen p x s = if p x then UnorderedList (x : toList s) else s

    -- | Implement the `union` operation for the unordered list representation
    -- of sets.
    union = foldl (flip adjoin)

instance (Show a) => Show (UnorderedList a) where
    show (UnorderedList elems) = "{" ++ go elems ++ "}"
      where
        go [] = ""
        go xs = foldr1 (\a b -> a ++ "," ++ b) $ map show xs

instance (Ord a) => Eq (UnorderedList a) where
    a == b = foldl (isMember a) True b && foldl (isMember b) True a
      where
        isMember set tf x = tf && x `isElem` set

instance (Ord a) => Ord (UnorderedList a) where
    compare = compare `on` toList

instance (Ord a) => Semigroup (UnorderedList a) where
    (<>) = union

instance (Ord a) => Monoid (UnorderedList a) where
    mempty = UnorderedList mempty

