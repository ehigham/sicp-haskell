{-# OPTIONS_GHC -Wno-unsafe #-}

module Chapter2.Exercise63
    (
        Tree (Empty, Node),
        treeToList1,
        treeToList2,
        fromList,
        fromOrderedSet,
        toOrderedSet
    ) where

import Data.Foldable (toList)
import Data.Function (on)

import Chapter2.Exercise61 (OrderedSet (OrderedSet))
import Chapter2.Set (Set (..))

data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving stock (Show)

-- | Each of the following two procedures converts a binary tree to a list
treeToList1 :: Tree a -> [a]
treeToList1 Empty        = []
treeToList1 (Node l x r) = treeToList1 l ++ (x : treeToList1 r)

treeToList2 :: Tree a -> [a]
treeToList2 = go []
  where
    go result Empty        = result
    go result (Node l x r) = go (x : go result r) l

-- | a. Do the two procedures produce the same result for every tree? If not,
-- how do the results differ? What lists do the two procedures produce for the
-- trees in figure 2.16
--
-- Both procedures produce the same result as the both traverse the tree
-- in-order.

-- | b. Do the two procedures have the same order of growth in the number of
-- steps required to convert a balanved tree with n elements to a list? If not,
-- which one grows more slowly?
--
-- For treeToList1:
-- T(N) = T(N/2) + O(N/2) + T(N/2) -- assume ++ is O(N)
-- T(N) = O(N log N)
--
-- For treeToList2
-- T(N) = T(N/2) + O(1) + T(N/2)
-- T(N) = O(N)

-- | Exercise 2.64
-- The following procedure fromList converts a an ordered list to a balanced
-- binary tree. The helper procedure takes as arguments an integer n and a list
-- of at least n elements and constructs a balanced tree containing the first
-- n elements of the list. The result returned by the helper is a pair whose
-- `fst` is the constructed tree and `snd` is the list of elements not included
-- in the tree
fromList :: [a] -> Tree a
fromList elems = fst $ partialTree elems (length elems)
 where
    partialTree xs 0 = (Empty, xs)
    partialTree xs n =
        let leftSize = (n - 1) `quot` 2
            (left, nonLeft) = partialTree xs leftSize
            (right, rest) = partialTree (tail nonLeft) (n - succ leftSize)
        in
            (Node left (head nonLeft) right, rest)

-- | a. Write a short paragraph explaining as clearly as you can how partialTree
-- works. Draw the tree produced by `fromList [1,3..11]`
--
-- `partialTree` splits the list into 2 parts roughly of length N/2. With these
-- partitions, it builds the left tree. With the rest of the elements, it creates
-- a binary tree whose entry is the first element not in the left tree and whose
-- right branch is made from the rest.
--
--                           5
--                      ____/ \____
--                     /           \
--                    1             9
--                  _/ \_         _/ \_
--                 /     \       /     \
--                .       3     7      11
--                       / \   / \     / \
--                      .   . .   .   .   .
--
-- >>> fromList [1, 3..11] :: Tree Int
-- Node
--     (Node
--         Empty
--         1
--         (Node Empty 3 Empty))
--      5
--      (Node
--          (Node Empty 7 Empty)
--           9
--           (Node Empty 11 Empty))

-- | b. What is the order of growth in the number of steps required by
-- `fromList` to convert a list o N elements?
--
-- At each step, partialTree "splits" the input into lists of roughly equal
-- length by computing a midpoint - O(1). It builds a tree from half its
-- input twice. So we have:
--
-- T(N) = 2 * T(N/2) + O(1)
-- T(N) = O(N)
--

-- | Exercise 2.65
-- Use the results of exercises 2.63 and 2.64 to give an O(N) implementation
-- for `union` and `intersect` for sets implemented as (balanced) binary trees

instance Set Tree where
    adjoin a Empty        = pure a
    adjoin a (Node l x r) | a == x    = Node l x r
                          | a < x     = Node (adjoin a l) x r
                          | otherwise = Node l x (adjoin a r)

    isElem _ Empty        = False
    isElem a (Node l x r) = isElem a l || a == x || isElem a r

    intersect Empty _     = Empty
    intersect _     Empty = Empty
    intersect a     b     = fromOrderedSet $ (intersect `on` toOrderedSet) a b

    union Empty s     = s
    union s     Empty = s
    union a     b     = fromOrderedSet $ (union `on` toOrderedSet) a b


toOrderedSet :: Tree a -> OrderedSet a
toOrderedSet = OrderedSet . toList

fromOrderedSet :: OrderedSet a -> Tree a
fromOrderedSet (OrderedSet xs) = fromList xs

instance (Eq a) => Eq (Tree a) where
    (==) = (==) `on` toList

instance Functor Tree where
    fmap _ Empty          = Empty
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
    foldr _ s Empty        = s
    foldr f s (Node l x r) = foldr f (f x (foldr f s r)) l

instance Applicative Tree where
    pure x = Node Empty x Empty

    Empty          <*> _              = Empty
    _              <*> Empty          = Empty
    (Node fl f fr) <*> (Node xl x xr) = Node (fl <*> xl) (f x) (fr <*> xr)

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
