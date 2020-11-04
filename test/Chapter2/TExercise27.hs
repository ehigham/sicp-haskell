{-# OPTIONS_GHC -Wno-type-defaults #-}
module Chapter2.TExercise27 (tests) where
    import Chapter2.Exercise24 (Tree (Leaf, Node))
    import Chapter2.Exercise27 (deepReverse)
    import Test.HUnit

    tests :: Test
    tests = TestList [
        TestLabel "empty" ((mempty :: Tree Int) ~=? (deepReverse mempty)),
        TestLabel "singleton" ((Leaf 1) ~=? (deepReverse (Leaf 1))),
        TestLabel "list" ((Node . reverse . map Leaf $ [1..3]) ~=? (deepReverse . Node . map Leaf $ [1..3])),
        TestLabel "tree" (Node [Node [Leaf 4, Leaf 3], Node [Leaf 2, Leaf 1]] ~=? (deepReverse $ Node [Node [Leaf 1, Leaf 2], Node [Leaf 3, Leaf 4]]))
        ]
