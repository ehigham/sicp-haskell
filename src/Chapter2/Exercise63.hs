module Chapter2.Exercise63
    (
        Tree (Empty, Leaf, Node),
        treeToList1,
        treeToList2,
        fromList
    ) where

import Control.Applicative (Alternative, (<|>), empty)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor ((<&>))

data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
    deriving stock (Eq, Ord, Show)

instance Functor Tree where
    fmap _ Empty        = Empty
    fmap f (Leaf x)     = Leaf (f x)
    fmap f (Node l x r) = Node (f <$> l) (f x) (f <$> r)

instance Foldable Tree where
    foldr _ s Empty        = s
    foldr f s (Leaf x)     = f x s
    foldr f s (Node l x r) = foldr f (f x (foldr f s r)) l

instance Applicative Tree where
    pure = Leaf

    Empty        <*> _            = Empty
    _            <*> Empty        = Empty
    Leaf f       <*> Leaf x       = Leaf (f x)
    Leaf f       <*> Node l x r   = Node (f <$> l)      (f x) (f <$> r)
    Node fl f fr <*> Leaf x       = Node (fl <&> ($ x)) (f x) (fr <&> ($ x))
    Node fl f fr <*> Node xl x xr = Node (fl <*> xl)    (f x) (fr <*> xr)

instance Alternative Tree where
    empty = mempty
    (<|>) = mappend

instance Monad Tree where
    Empty      >>= _ = Empty
    Leaf x     >>= f = f x
    Node l x r >>= f = (l >>= f) `mappend` f x `mappend` (r >>= f)

instance Traversable Tree where
    traverse _ Empty        = pure Empty
    traverse f (Leaf x)     = Leaf <$> f x
    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r

instance Semigroup (Tree a) where
    (<>) = (fromList .) . (mappend `on` toList)

instance Monoid (Tree a) where
    mempty  = Empty

-- | Each of the following two procedures converts a binary tree to a list
treeToList1 :: Tree a -> [a]
treeToList1 Empty        = []
treeToList1 (Leaf x)     = [x]
treeToList1 (Node l x r) = treeToList1 l ++ (x : treeToList1 r)

treeToList2 :: Tree a -> [a]
treeToList2 = go []
  where
    go result Empty        = result
    go result (Leaf x)     = x : result
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
    partialTree xs     0 = (Empty, xs)
    partialTree (x:xs) 1 = (Leaf x, xs)
    partialTree xs     n =
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
--                        __/ \__
--                       /       \
--                      1         9
--                    _/ \_     _/ \_
--                   /     \   /     \
--                  .       3 7      11
--
-- >>> fromList [1, 3..11] :: Tree Int
-- Node
--     (Node Empty 1 (Leaf 3))
--      5
--      (Node (Leaf 7) 9 (Leaf 11)))

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
