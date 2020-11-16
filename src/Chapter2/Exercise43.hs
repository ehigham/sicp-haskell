{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter2.Exercise43 () where

import Chapter1.Utilities (timeIt)
import Chapter2.Exercise42 (queens)

-- | Louis Reasoner is having a terrible time doing exercise 2.42. His `queens`
-- procedure seems to work, but it runs extremely slowly (Louis never does
-- manage to wait long enough for it to solve even the 6x6 case). When Louis
-- asks Eva Lu Ator for help, she points out that he has interchanged the order
-- of the nested mappings in `flatMap`, writing it as
queens' :: Integer -> [[(Integer, Integer)]]
queens' n = go n
  where
    go 0 = [[]]
    go k = filter isSafe $ do
        position <- [(k, i) | i <- [1..n]]
        restOfQueens <- go (pred k)
        return $ adjoin position restOfQueens
    adjoin = (:)
    isSafe []     = True
    isSafe (q:qs) = not $ any (checks q) qs
    checks (x, y) (p, q) = x == p || y == q || abs (x - p) == abs (y - q)

-- | Explain why this interchange makes the program run slowly. Estimate how
-- long it will take Louis' program to solve the eight-queens puzzle, assuming
-- that the program in exercise 2.42 solves the puzzle in time T.
--
-- In Louis' version, the recursive call to `go` is re-evaluated for each item
-- in the range [(k, i) | i <- [1..n]]. Since the minimum number of recursions
-- is n, then this work is duplicated n-by-n times. Therefore, if it takes T for
-- the original program for a board size of 8, then Louis' program would take
-- ~16T.
--
-- Note that Haskell, being a pure functional language, does not have this
-- problem as the compiler *can* re-arrange the evaluation order of pure
-- functions without changing the meaning of the program (a property known as
-- referential transparency). Running the following in ghci, however, shows
-- a measurable performance difference.
perfTest :: IO ()
perfTest = do
    putStr "timing queens procedure: "
    (_, t0) <- timeIt $ queens 8
    print  t0
    putStr "timing Louis' procedure: "
    (_, t1) <- timeIt $ queens' 8
    print t1

-- >>> perfTest
-- timing queens procedure: 0.002309635s
-- timing Louis' procedure: 5.92556229s
