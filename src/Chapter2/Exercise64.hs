module Chapter2.Exercise64 (listToTree) where

import Chapter2.Exercise63 (Tree (Empty, Node))

-- | The following procedure listToTree converts a an ordered list to a balanced
-- binary tree. The helper procedure takes as arguments an integer n and a list
-- of at least n elements and constructs a balanced tree containing the first
-- n elements of the list. The result returned by the helper is a pair whose
-- `fst` is the constructed tree and `snd` is the list of elements not included
-- in the tree
listToTree :: [a] -> Tree a
listToTree elems = fst $ partialTree elems (length elems)
 where
    partialTree xs 0 = (Empty, xs)
    partialTree xs n =
        let leftSize = (n - 1) `quot` 2
            (left, nonLeft) = partialTree xs leftSize
            (right, rest) = partialTree (tail nonLeft) (n - succ leftSize)
        in
            (Node left (head nonLeft) right, rest)

-- | a. Write a short paragraph explaining as clearly as you can how partialTree
-- works. Draw the tree produced by `listToTree [1,3..11]`
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
-- >>> listToTree [1, 3..11] :: Tree Int
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
-- `listToTree` to convert a list o N elements?
--
-- At each step, partialTree "splits" the input into lists of roughly equal
-- length by computing a midpoint - O(1). It builds a tree from half its
-- input twice. So we have:
--
-- T(N) = 2 * T(N/2) + O(1)
-- T(N) = O(N)
--
