module Chapter1.Exercise11 (f, f') where
-- | A function `f` is defined by the rule that
-- |
-- |     f(n) = n                                 ,  if n < 3
-- |            f(n - 1) + 2*f(n - 2) + 3*f(n - 3),  if n >= 3
-- |
-- | Write a procedure that computes `f` by means of an recursive process:
    f :: Int -> Int
    f n = if n < 3 then n else sum . fmap g $ [1, 2, 3]
        where g = \x -> x * f (n - x)

-- | Write a procedure that computes `f` by means of an iterative process:
    f' :: Int -> Int
    f' = fIter (2, 1, 0)
        where fIter = \(a, b, c) -> \count -> if count == 0 then c
                else fIter ((a + 2*b + 3*c), a, b) (count - 1)
