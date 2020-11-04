module Chapter2.Exercise27 (deepReverse) where
    import Chapter2.Exercise24 (Tree (Node))

    deepReverse :: Tree a -> Tree a
    deepReverse (Node xs) = Node $ foldl (flip ((:) . deepReverse)) [] xs
    deepReverse leaf = leaf
