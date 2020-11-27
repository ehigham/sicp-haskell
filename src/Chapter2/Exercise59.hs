module Chapter2.Exercise59 (UnorderedSet(UnorderedSet)) where

import Control.Applicative (Alternative, (<|>), empty)

import Chapter2.Set (Set(..))

newtype UnorderedSet a = UnorderedSet [a]

instance Set UnorderedSet where
    adjoin x s = if x `isElem` s then s else pure x <> s

    isElem a (UnorderedSet elems) = go elems
      where
        go []     = False
        go (x:xs) = a == x || go xs

    intersect a b = a >>= \x -> if x `isElem` b then pure x else empty

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

instance Functor UnorderedSet where
    fmap f (UnorderedSet xs) = UnorderedSet $ fmap f xs

instance Applicative UnorderedSet where
    pure x = UnorderedSet [x]
    (UnorderedSet fs) <*> (UnorderedSet xs) = UnorderedSet $ fs <*> xs

instance Alternative UnorderedSet where
    empty = mempty
    (UnorderedSet xs) <|> (UnorderedSet ys) = UnorderedSet $ xs <|> ys

instance Monad UnorderedSet where
    (UnorderedSet xs) >>= f = foldMap f xs

instance Foldable UnorderedSet where
    foldr f s (UnorderedSet elems) = foldr f s elems

instance Semigroup (UnorderedSet a) where
    (UnorderedSet xs) <> (UnorderedSet ys) = UnorderedSet (xs <> ys)

instance Monoid (UnorderedSet a) where
    mempty = UnorderedSet []

    --isElem _ []     = False
    --isElem a (x:xs) = a == x || a `isElem` xs

