{-# LANGUAGE FlexibleInstances #-}
module Chapter2.Exercise20 (sameParity) where
-- | In Scheme, the procedures `+`, `*` and `list` take arbitrary numbers
-- of arguments. One way to define such procedures is to use `define` with
-- "dotted-tail" notation. In a procedure definition, a parameter list that
-- has a dot before the final parameter name indicates that, when a procedure
-- is called, the initial parameters (if any) will have as values the initial
-- arguments but the final parameter's value will be a `list` of any
-- remaining arguments. For instance, given the definition
--
--   (define (f x y . z) <body>)
--
-- The procedure `f` can be called with two or more arguments. If we evaluate
-- `(f 1 2 3 4 5 6)`, then in the body of `f`, `x` will be `, `y` will be 2,
-- and `z` will be the list [3, 4, 5, 6].
-- Given the definition
--
--   (define (g . w) <body>)
--
-- the procedure `g` can be called with zero or more arguments. If we evaluate
-- `(g 1 2 3 4 5 6)`, then in the body of `g`, `w` will be the list
-- [1, 2, 3, 4, 5, 6].
--
-- Use this notation to write a procdure `sameParity` that takes one or more
-- integers and returns a list of all the arguments that have the same
-- even-odd parity as the first argument. For example:
--
-- >>> sameParity 1 2 3 4 5 6 7
-- [1, 3, 5, 7]
--
-- >>> sameParity 2 3 4 5 6 7
-- [2, 4, 6]

-- | In Haskell, we can't just use "dotted-tail" notation. Instead we can do a
-- trick with type clases to return either an `Integer` or a function.

class ParityResult r where
    parity :: (Integer -> Bool) -> [Integer] -> r

instance ParityResult [Integer] where
    parity = (reverse .) . filter

instance (Integral a, ParityResult r) => ParityResult (a -> r) where
    parity f xs x = parity f (toInteger x:xs)

sameParity :: (ParityResult r) => Integer -> r
sameParity x = parity (if even x then even else odd) [x]
