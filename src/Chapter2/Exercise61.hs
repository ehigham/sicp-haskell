{-# LANGUAGE DerivingVia #-}

module Chapter2.Exercise61 (OrderedList) where

import Data.Foldable (toList)
import Chapter2.Set (Set(..))

newtype OrderedList a = OrderedList [a]
  deriving stock (Eq, Ord)
  deriving Foldable via []

instance Set OrderedList where
    -- | Give an implementation of `adjoin` using the ordered representation.
    -- By analogy with isElem show how to take advantage of the ordering to
    -- produce a procedure that requires on average about half as many steps as
    -- with the unordered representation
    adjoin a = OrderedList . go [] . toList
      where
        go stack []     = rebuild stack [a]
        go stack (x:xs) | a == x    = rebuild stack (x:xs)
                        | a < x     = rebuild stack (a:x:xs)
                        | otherwise = go (x:stack) xs

    isElem a = go . toList
      where
        go []     = False
        go (x:xs) | a == x    = True
                  | a < x     = False
                  | otherwise = go xs

    fromList = foldr adjoin mempty

    intersect a b = OrderedList $ go (toList a) (toList b)
      where
        go [] _          = mempty
        go _ []          = mempty
        go (x:xs) (y:ys) | x == y    = x : go xs ys
                         | x < y     = go xs (y:ys)
                         | otherwise = go (x:xs) ys

    -- | Exercise 2.62
    -- Give a Theta(N) implementation of `union` for sets represented as
    -- ordered lists.
    union a b = OrderedList $ go [] (toList a) (toList b)
      where
        go stack [] ys         = rebuild stack ys
        go stack xs []         = rebuild stack xs
        go stack (x:xs) (y:ys) | x == y    = go stack xs ys
                               | x < y     = go (x:stack) xs (y:ys)
                               | otherwise = go (y:stack) (x:xs) ys

-- rebuild the ordered list by appending the rest to the reversed stack
rebuild stack rest = foldl (flip (:)) rest stack

instance (Show a) => Show (OrderedList a) where
    show s = "{" ++ go (toList s) ++ "}"
      where
        go [] = ""
        go xs = foldr1 (\a b -> a ++ "," ++ b) $ map show xs

instance (Ord a) => Semigroup (OrderedList a) where
    (<>) = union

instance (Ord a) => Monoid (OrderedList a) where
    mempty = OrderedList mempty

