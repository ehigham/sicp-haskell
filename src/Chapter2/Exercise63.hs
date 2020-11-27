module Chapter2.Exercise63
    (
        Tree (Empty, Node),
        entry,
        left,
        right,
        treeToList1,
        treeToList2
    ) where

data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving stock (Show, Eq)

entry :: Tree a -> a
entry Empty          = error "empty tree"
entry (Node _ a _) = a

left, right :: Tree a -> Tree a
left  Empty          = error "empty tree"
left  (Node l _ _) = l
right Empty          = error "empty tree"
right (Node _ _ r) = r

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
