{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter1.Exercise34 () where
-- | Suppose we define the procedure:
f :: (Num a) => (a -> b) -> b
f = ($ 2)

-- | Then we have
-- >>> f square
-- 4
--
-- >>> f $ ap (*) (1 +)
-- 6

-- | What happens if we (perversely) ask the interpreter to evaluate the
-- combination f f? Explain.
-- >>> f f
-- ($ 2) f
-- f 2
-- ($ 2) 2
-- 2 2
-- | Error!
-- In reality, Haskell's type-checker complains
