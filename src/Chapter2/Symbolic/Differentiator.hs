{-# LANGUAGE QuasiQuotes #-}
module Chapter2.Symbolic.Differentiator (deriv) where

import Chapter2.Symbolic.Expr (Expr(Const, Var), expr)

deriv :: Expr -> String -> Expr
deriv (Const _)       _   = [expr|0|]
deriv (Var x)         var = if x == var then [expr|1|] else [expr|0|]
deriv [expr|$f + $g|] var = [expr|$f' + $g'|]
  where
    f' = deriv f var
    g' = deriv g var
deriv [expr|$f - $g|] var = [expr|$f' - $g'|]
  where
    f' = deriv f var
    g' = deriv g var
deriv [expr|$f * $g|] var = [expr|$f' * $g + $f * $g'|]
  where
    f' = deriv f var
    g' = deriv g var
deriv [expr|$f / $g|] var = [expr|($f' * $g - $f * $g')/ $g^2|]
 where
    f' = deriv f var
    g' = deriv g var
deriv [expr|-$f|]     var = [expr|-$f'|]
  where
    f' = deriv f var
deriv [expr|+$f|]     var = [expr|+$f'|]
  where
    f' = deriv f var
deriv [expr|ln $f|]   var = [expr|1 / $f * $f'|]
  where
    f' = deriv f var
-- | Exercise 2.56
-- Show how to extend the basic `deriv` program to handle more kinds of
-- expressions. For instance, implement the differentiation rule:
-- @
--   d(f^g)/dx = f^(g-1) * (g * df/dx + f dg/dx ln f)
-- @
deriv [expr|$f ^ $g|] var = [expr|$f^($g - 1)*($g*$f' + $f*(ln $f)*$g')|]
  where
    f' = deriv f var
    g' = deriv g var
deriv _               _   = error "unknown expression type"


-- | Exercise 2.57
-- Suppose we want to modify the differentiation program so it works with
-- ordinary mathematical notation, in which `+` and `*` are infix rather than
-- prefix operators. Since the differentation program is defined in terms of
-- abstract data, we cna modify it to work with different representations of
-- expressions solely by chaning the predicates, selectors and constructors that
-- define the representation of algebraic expressions on which the differentator
-- is to operate.
--
-- a. Show how to do this in order to differentiate algebraic expressions
-- presented in infix form, such as
-- @
--     (x + (3 * (x + (y + 2))))
-- @
-- To Simplify the task, assume that + and * always take two arguments and that
-- expressions are fully parenthesised,
--
-- In this implementation, we just change the grammer (and parser thereof) of
-- our expressions. Since expressions are parenthesised, we don't need to worry
-- about operator precedence. Using definitions from `Chapter2.Symbolic.Expr`:
-- @
--     expression :: Parser Expr
--     expression  = integer <|> variable <|> parens application
--       where
--         application = do a <- expression
--                          f <- add <|> sub <|> multiply <|> divide
--                          f a <$> expression
-- @
--
-- b. The problem becomes substantially harder if we allow standard algebraic
-- notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parenthesis
-- and assumes that multiplication is done before addition. Can you design
-- appropriate predicates, selectors and constructors for this notation such
-- that out derivative program still works?
--
-- Complete via grammar implementation.
