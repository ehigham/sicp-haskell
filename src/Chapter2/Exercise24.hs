module Chapter2.Exercise24 (
    Tree (Leaf, Node),
    countLeaves,
    BoxAndPointer
) where

-- | Simple representation of a tree-like data structure
    data Tree a = Leaf a | Node [Tree a]
        deriving stock (Eq)

-- | Suppose we evaluated the expression
-- | `Node [Leaf 1, Node [Leaf 2, Node [Leaf 3, Leaf 4]]] :: Tree Integer`
-- | Give the result printed by the intepreter, the corresponding box-and-
-- | pointer structure and the interpretation of this as a tree (as in
-- | figure 2.6).
-- >>> instance BoxAndPointer Integer
-- >>> Node [Leaf 1, Node [Leaf 2, Node [Leaf 3, Leaf 4]]] :: Tree Integer
-- "[1, [2, [3, 4]]]"

-- >>> instance BoxAndPointer Integer
-- >>> draw (Node [Leaf 1, Node [Leaf 2, Node [Leaf 3, Leaf 4]]] :: Tree Integer)
-- "[[1]->[[[2]->[[[3]->[[4]->/]]->/]]->/]]"

-- | As a tree:
-- |
-- |     *
-- |    / \
-- |   1   *
-- |      / \
-- |     2   *
-- |        / \
-- |       3   4

    instance (Show a) => Show (Tree a) where
        show (Leaf x) = show x
        show (Node xs) = show xs

    instance Functor Tree where
        fmap f (Leaf x) = Leaf $ f x
        fmap f (Node xs) = Node $ (fmap . fmap) f xs

    instance Applicative Tree where
        pure = Leaf
        (Leaf f) <*> t = fmap f t
        (Node fs) <*> t = Node $ fmap (<*> t) fs

    instance Monad Tree where
        (Leaf x) >>= f = f x
        (Node xs) >>= f = Node $ fmap (>>= f) xs

    instance Foldable Tree where
        foldr f s (Leaf x) = f x s
        foldr f s (Node xs) = foldr (flip (foldr f)) s xs

    instance Traversable Tree where
        traverse f (Leaf x) = Leaf <$> f x
        traverse f (Node xs) = Node <$> traverse (traverse f) xs

    instance Semigroup (Tree a) where
        x <> y = Node $ filter (not . null) [x, y]

    instance Monoid (Tree a) where
        mempty = Node []

    class Show a => BoxAndPointer a where
        draw :: a -> String
        draw x = "[" ++ show x ++ "]"

    instance (BoxAndPointer a) => BoxAndPointer ([] a) where
        draw [] = "/"
        draw (x:xs) = "[" ++ draw x ++ "->" ++ draw xs ++ "]"

    instance (BoxAndPointer a) => BoxAndPointer (Tree a) where
        draw (Leaf x) = draw x
        draw (Node xs) = draw xs

    countLeaves :: Tree a -> Int
    countLeaves = length
