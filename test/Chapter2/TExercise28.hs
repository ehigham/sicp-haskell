module Chapter2.TExercise28 (tests) where
    import Chapter2.Exercise24 (Tree (Node, Leaf))
    import Chapter2.Exercise28 (fringe)
    import Test.HUnit

    tests :: Test
    tests = TestList [
        TestLabel "list" (xs ~=? (fringe tree)),
        TestLabel "tree" (xs <> xs ~=? (fringe $ tree <> tree))]
      where
        xs = [1..4] :: [Int]
        tree = Node [Node [Leaf 1, Leaf 2], Node [Leaf 3, Leaf 4]] :: Tree Int
