module Chapter2.Exercise28 (fringe) where
-- | Write a procedure fringe that takes as argument a `Tree` as returns a
-- | `List` whose elements are all the leaves of the tree arranged in left-
-- | to-right order.
    import Chapter2.Exercise24 (Tree)
    import Data.Foldable (toList)

    fringe :: Tree a -> [a]
    fringe = toList
