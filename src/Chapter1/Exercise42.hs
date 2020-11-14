module Chapter1.Exercise42 (compose) where
-- | let `f` and `g` be two unary functions.The 'composition' `f` after `g` is
-- defined to be the function \x -> f (g x). Define a procedure `compose` that
-- implements composition. For example, if `inc` is a procedure that adds 1 to
-- its argument:
-- >>> compose square inc $ 6
-- 49

-- Function composition (.) is build into the Haskell language
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = (.)
