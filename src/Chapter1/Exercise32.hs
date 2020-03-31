module Chapter1.Exercise32 (accumulate, sum, product) where
    import Prelude hiding (sum, product)
-- | a. Show that `sum` and `product` are both special cases of a still more
-- | general notion called `accumulate` that combines a collection of terms
-- | using some general accumulation function:

    accumulate :: (Ord a) => (b -> b -> b) -> b -> (a -> b) -> a -> (a -> a) -> a -> b

-- | `accumulate` takes as arguments the same term and range specifications as
-- | `sum` and `product`, together with a `combine` procedure (of two arguments)
-- | that specifies how the current term is to be combined with the accumulation
-- | of the preceding terms and a `state` value that specifies what base value
-- | to use when the terms run out. Write `accumulate` and show how `sum` and
-- | `product` can both be defined as simple calls to `accumulate`.

    accumulate combine state term a next b = go a state
      where
        go x acc | x > b     = acc
                 | otherwise = go (next x) $ combine acc (term x)

    sum :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
    sum = accumulate (+) 0

    product :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
    product = accumulate (*) 1

-- | b. If your `accumulate`  procedure generates a recursive process, write one
-- | that makes an iterative process. If it generates an iterative process,
-- | write one that generates a recursive process.

    accumulate' :: (Ord a) => (b -> b -> b) -> b -> (a -> b) -> a -> (a -> a) -> a -> b
    accumulate' combine state term a next b = go a
      where
        go x | x > b     = state
             | otherwise = combine (term x) $ go (next x)
