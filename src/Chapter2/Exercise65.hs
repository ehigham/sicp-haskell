
{-# LANGUAGE DerivingVia #-}

module Chapter2.Exercise65 (TreeSet, toTree) where

import Data.Foldable (toList)
import Data.Function (on)

import           Chapter2.Exercise61 (OrderedList)
import           Chapter2.Exercise63 (Tree(..))
import qualified Chapter2.Exercise63 as T (fromList)
import           Chapter2.Set (Set (..))

-- Use the results of exercises 2.63 and 2.64 to give an O(N) implementation
-- for `union` and `intersect` for sets implemented as (balanced) binary trees

newtype TreeSet a = TreeSet { toTree :: Tree a  }
  deriving Foldable via Tree

instance Set TreeSet where
    adjoin a = TreeSet . go . toTree
      where
        go Empty        = Leaf a
        go (Leaf x)     | a == x    = Leaf x
                        | a < x     = Node (Leaf a) x Empty
                        | otherwise = Node Empty x (Leaf a)
        go (Node l x r) | a == x    = Node l x r
                        | a < x     = Node (toTree $ adjoin a (TreeSet l)) x r
                        | otherwise = Node l x (toTree $ adjoin a (TreeSet r))

    isElem a = go . toTree
      where
        go Empty        = False
        go (Leaf x)     = a == x
        go (Node l x r) = a == x || go l || go r

    fromList = TreeSet . T.fromList

    intersect a b
        | null a    = mempty
        | null b    = mempty
        | otherwise = fromOrderedList $ (intersect `on` toOrderedList) a b

    union a b
        | null a    = b
        | null b    = a
        | otherwise = fromOrderedList $ (union `on` toOrderedList) a b

instance (Eq a) => Eq (TreeSet a) where
    (==) = (==) `on` toList

instance (Ord a) => Ord (TreeSet a) where
    compare = compare `on` toList

instance (Show a) => Show (TreeSet a) where
    show (TreeSet s) = "{" ++ go (toList s) ++ "}"
      where
        go [] = ""
        go xs = foldr1 (\a b -> a ++ "," ++ b) $ map show xs

instance (Ord a) => Semigroup (TreeSet a) where
    (<>) = union

instance (Ord a) => Monoid (TreeSet a) where
    mempty = TreeSet Empty

toOrderedList :: (Ord a) => TreeSet a -> OrderedList a
toOrderedList = fromList . toList

fromOrderedList :: (Ord a) => OrderedList a -> TreeSet a
fromOrderedList = fromList . toList
