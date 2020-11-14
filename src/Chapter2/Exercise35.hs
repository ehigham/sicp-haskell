module Chapter2.Exercise35 (countLeaves) where

import Chapter2.Exercise24 (Tree)

-- | Redefine `countLeaves` from section 2.2.2 as an accumulation
countLeaves :: Tree a -> Integer
countLeaves = foldr (+) 0 . fmap (const 1)  -- NB: foldr (+) 0 == sum
