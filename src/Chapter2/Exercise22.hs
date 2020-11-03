{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter2.Exercise22 () where
    import Chapter1.Utilities (square)
-- | Louis Reasoner tries to rewrite the first `squareList` procedure of
-- | exercise 2.21 so that if follows an iterative process
    squareList :: (Num a) => [a] -> [a]
    squareList = go []
      where
        go ans [] = ans
        go ans (x:xs) = go ((square x):ans) xs

-- | Unfortunately, defining squareList this way produces the answer list in
-- | the reverse order of the one desired. Why?
-- | Because the `cons` operator `(:)` adds an element to the head of the list.
-- >>> squareList [1, 2, 3] :: [Integer]
-- [9,4,1]

