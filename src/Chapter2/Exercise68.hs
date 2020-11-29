module Chapter2.Exercise68 (encode) where

import Chapter2.Exercise63 (Tree (Leaf, Node))
import Chapter2.Exercise67 (HLeaf(symbols), HuffmanTree(Huffman), hGet)
import Chapter2.Set (isElem)

-- | The `encode` procedure takes as arguments a message and a tree and
-- produces a list of bits that gives teh encoded message
encode :: (Ord a, Show a) => HuffmanTree a -> [a] -> String
encode (Huffman tree) = foldMap (encodeChar tree)

-- | `encodeChar` is a procedure (which you must write) that returns the list
-- of bits that encodes a given symbol according to a given tree. You should
-- design `encodeChar` so that it signals an error if the symbol is not in the
-- tree at all.
encodeChar :: (Ord a, Show a) => Tree (HLeaf a) -> a -> String
encodeChar (Leaf x) c
    | c `isElem` symbols x = []
    | otherwise            = error ("illegal element: " ++ show c)
encodeChar (Node l _ r) c
    | c `isElem` hGet symbols mempty l = '0' : encodeChar l c
    | otherwise                        = '1' : encodeChar r c
encodeChar _ _ = error "logic error"


-- | Test yout procedure by encoding the result you obtained in exercise 2.67
-- with the sample tree and seeing whether it is the same as the original
-- sample message.
--
-- See test/Chapter2/TExercise68.hs

