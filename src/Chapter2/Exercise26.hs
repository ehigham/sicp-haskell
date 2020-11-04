{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter2.Exercise26 () where

-- | Suppose we define x and y to be two lists:
    x, y :: [Integer]
    x = [1..3]
    y = [4..6]

-- | What is the result printed by the intepreter in reponse to evaluating each
-- | of the following expressions:
-- >>> mappend x y
-- [1,2,3,4,5,6]

-- >> x : y -- you can't actually do this, but if you could...
-- ([1,2,3], [4,5,6])

-- >>> [x, y]
-- [[1,2,3],[4,5,6]]
