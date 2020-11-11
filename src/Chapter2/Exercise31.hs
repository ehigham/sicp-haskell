module Chapter2.Exercise31 (treeMap) where
    import Chapter2.Exercise24 (Tree)
-- | Abstract your answer to exercise 2.30 to produce a procedure `treeMap`
-- | with the property that `squareTree` could be defined as
-- >>> import Control.Monad (join)
-- >>> squareTree = treeMap (join (*))

    treeMap :: (a -> b) -> Tree a -> Tree b
    treeMap = fmap
