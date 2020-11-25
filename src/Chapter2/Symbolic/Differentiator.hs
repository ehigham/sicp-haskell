{-# LANGUAGE QuasiQuotes #-}
module Chapter2.Symbolic.Differentiator (deriv) where

import Chapter2.Symbolic.Expr (Expr(Const, Var), expr)

deriv :: Expr -> Expr -> Expr
deriv (Const _)       _         = [expr|0|]
deriv [expr|x|]       [expr|x|] = [expr|1|]
deriv (Var _)         [expr|x|] = [expr|0|]
deriv [expr|$f + $g|] var       = [expr|$f' + $g'|]
  where
    f' = deriv f var
    g' = deriv g var
deriv [expr|$f - $g|] var       = [expr|$f' - $g'|]
  where
    f' = deriv f var
    g' = deriv g var
deriv [expr|$f * $g|] var       = [expr|$f' * $g + $f * $g'|]
  where
    f' = deriv f var
    g' = deriv g var
deriv [expr|$f / $g|] var       = [expr|($f' * $g - $f * $g')/ $g^2|]
 where
    f' = deriv f var
    g' = deriv g var
deriv [expr|-$f|]     var       = [expr|-$f'|]
  where
    f' = deriv f var
deriv [expr|+$f|]     var       = [expr|+$f'|]
  where
    f' = deriv f var
deriv [expr|ln $f|]   var       = [expr|1 / $f * $f'|]
  where
    f' = deriv f var
-- | Exercise 2.56
-- Show how to extend the basic `deriv` program to handle more kinds of
-- expressions. For instance, implement the differentiation rule:
-- @
--   d(f^g)/dx = f^(g-1) * (g * df/dx + f dg/dx ln f)
-- @
deriv [expr|$f ^ $g|] var       = [expr|$f^($g - 1)*($g*$f' + $f*(ln $f)*$g')|]
  where
    f' = deriv f var
    g' = deriv g var
deriv _               _         = error "unknown expression type"
