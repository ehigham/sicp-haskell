{-# OPTIONS_GHC -Wno-unsafe #-}

module Chapter2.Exercise69 (huffman) where

import Chapter2.Exercise63 (Tree(Leaf))
import Chapter2.Exercise67 (HuffmanTree(Huffman), HLeaf(HLeaf))

-- | The following procedure takes as its argument a list of frequency-symbol
-- pairs (where no symbol appears in more than one pair) and generates a
-- Huffman encoding tree according to the Huffman algorithm.
huffman :: (Ord a) => [(Integer, a)] -> HuffmanTree a
huffman = successiveMerge . makeLeafSet

-- | `makeLeafSet` is a procedure that transforms the list of pairs into
-- an ordered set of leaves. `successiveMerge` is the procedure you must write
-- using `makeCodeTree` to successively merge the smallest-weight elements of
-- the set until there is only one element left, which is the desired Huffman
-- tree. (This procedure is a little tricky but not really complicated. If you
-- find yourself desiging a complex procedure, then you are almost certainly
-- doing something wrong. You can take significant advantage of the face that
-- we are using ordered set representation).

makeLeafSet :: [(Integer, a)] -> [HuffmanTree a]
makeLeafSet = fmap (\(w, s) -> Huffman $ Leaf (HLeaf w (pure s)))

successiveMerge :: (Ord a) => [HuffmanTree a] -> HuffmanTree a
successiveMerge = foldr1 (<>)
