module Chapter2.Exercise5 (cons, car, cdr) where

import Chapter1.Utilities (divides)

-- | Show that we can represent pairs of nonnegative integers using only numbers
-- and arithmetic operations if we represent the pair a b as the integer that
-- is the product `2^a . 3^b`. Give the corresponding definitions of the
-- procedures `car` and `cdr`

newtype Pair = Pair Integer

cons :: Integer -> Integer -> Pair
cons a b = Pair $ 2^a * 3^b

car :: Pair -> Integer
car = countEvenDivisions 2

cdr :: Pair -> Integer
cdr = countEvenDivisions 3

instance Show Pair where
    show p = "(" ++ show (car p) ++ ", " ++ show (cdr p) ++ ")"

instance Eq Pair where
    (==) (Pair x) (Pair y) = x == y

countEvenDivisions :: Integer -> Pair -> Integer
countEvenDivisions base (Pair p) = go 0 p
  where
    go count x | base `divides` x = go (succ count) (x `div` base)
               | otherwise        = count
