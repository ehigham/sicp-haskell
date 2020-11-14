{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter2.Exercise30 () where

import Chapter1.Utilities (square)
import Chapter2.Exercise24 (Tree (Leaf, Node))

-- | Define a procedure `squareTree` analogous to the `squareList` procedure
-- of exercise 2.21. Define `squareTree` directly (i.e. without using any
-- higher-order procedures) and also by using map and recusion.

squareTree :: (Num a) => Tree a -> Tree a
squareTree (Leaf x) = Leaf $ square x
squareTree (Node xs) = Node $ go xs
  where
    go [] = []
    go (x:rest) = squareTree x : go rest

squareTree' :: (Num a) => Tree a -> Tree a
squareTree' (Leaf x) = Leaf $ square x
squareTree' (Node xs) = Node $ map squareTree xs
