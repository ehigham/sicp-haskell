{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter1.Exercise31 (product, factorial) where
import Prelude hiding (product)

-- | a. The `sum` procedure is only the simplest of a vast number of similiar
-- abstractions that can be captured as higher-order procedures. Write an
-- analogous procedure called `product` that returns the product of the
-- values of a function at points over a given range, Show how to define
-- `factorial` in terms of product. Also use `product` to compute
-- approximations to pi using the formula:
-- @
--     pi `div` 4 = (2.4.4.6.6.8 ..) `div` (3.3.5.5.7.7 ...)
-- @
product :: (Num b, Ord a) => (a -> b) -> a -> (a -> a) -> a -> b
product term a next b = go a 1
  where
    go x res | x > b     = res
             | otherwise = go (next x) $ res * term x

factorial :: (Integral a) => a -> a
factorial = product id 1 (+1)

approxPi :: (Integral a) => a -> Double
approxPi = (*4.0) . product term 1 (+1)
  where
    term x | even x    = fromIntegral (x + 2) / fromIntegral (x + 1)
           | otherwise = fromIntegral (x + 1) / fromIntegral (x + 2)

-- >>> approxPi 10
-- 3.2751010413348065
--
-- >>> approxPi 100
-- 3.1570301764551654
--
-- >>> approxPi 1000
-- 3.1431607055322552

-- | b. If your `product` procedure generates a recursive process, write one
-- | that generates an iterative process. If it generates an iterative process,
-- | write one that generates a recursive process
product' :: (Num b, Ord a) => (a -> b) -> a -> (a -> a) -> a -> b
product' term a next b = go a
  where
    go x | x > b     = 1
         | otherwise = (*) (term x) $ go (next x)
