{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds #-}
module Chapter1.Exercise5 () where
-- | Ben Bitdiddle has invented a test to determine whether the interpreter
-- | he is faced with using applicative-order evaluation or normal-order
-- | evaluation. He defines the following two procedures:
    p = p
    test x y = if x == 0 then 0 else y

-- | Then he evaluates the expression
-- >>> main :: IO ()
-- >>> main = print $ test 0 p
--
-- | What behaviour will Ben observe with an interpreter that uses applicative-
-- | order evaluation? What behaviour will Ben observe with an interpreter that
-- | uses normal-order evaluation? Explain your answer.
--
-- | Applicative-Ordering:
-- | Program will recurse infinitely, as evaluation of the arugments to test
-- | results in the evaluation of p ad infinitum
--
-- | Normal-Ordering:
-- | Program will terminate successfully (printing 0) as p is never evaluated
