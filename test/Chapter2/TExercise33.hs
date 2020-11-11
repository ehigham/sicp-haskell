module Chapter2.TExercise33 (tests) where
    import Chapter1.Utilities (square)
    import Chapter2.Exercise24 (Tree (Leaf, Node))
    import Chapter2.Exercise33 (map', append', length')
    import TestTools ((~/=?))

    import Control.Applicative (Alternative, (<|>))
    import Test.HUnit

    tests :: Test
    tests = TestList [testMap', testAppend', testLength']

    ints :: [Integer]
    ints = [1..10]

    tree :: Tree Integer
    tree = Node [Leaf 1, Node [Leaf 2, Leaf 3], Leaf 4]

    mx :: Maybe Integer
    mx = Just 2

    testMap' :: Test
    testMap' = TestList [
        TestLabel "testing []" $ (fmap square ints) ~=? (map' square ints),
        TestLabel "testing Maybe" $ (fmap square mx) ~=? (map' square mx),
        TestLabel "fails on Trees" $ (fmap square tree) ~/=? (map' square tree)]

    testAppend' :: Test
    testAppend' = TestList [
        TestLabel "testing []" $ mkTest ints,
        TestLabel "testing Maybe" $ mkTest mx,
        TestLabel "fails on Trees" $ (tree <> tree) ~/=? (append' tree tree)]
      where
        mkTest :: (Alternative t, Foldable t, Eq (t Integer), Show (t Integer))
                    => t Integer -> Test
        mkTest xs = let ys = fmap (* 2) xs in (xs <|> ys) ~=? (append' xs ys)

    testLength' :: Test
    testLength' = TestList [
        TestLabel "testing []" $ 0 ~=? length' [],
        TestLabel "testing ints" $ 10 ~=? length' ints,
        TestLabel "testing Maybe" $ 1 ~=? length' mx,
        TestLabel "testing Trees" $ 4 ~=? length' tree]
