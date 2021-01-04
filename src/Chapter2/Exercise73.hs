module Chapter2.Exercise73 (deriv) where

import Chapter2.Symbolic
    (
        Expr(Const, Var, BinaryExpr, UnaryExpr),
        BinaryOp(Add, Sub, Mul, Pow),
        UnaryOp(Ln)
    )
import Data.Map (Map, (!), empty, fromList)

-- | Section 2.3.2 described a program that performs symbolic differentiation.
-- We can regard this program as performing a dispatch on the type of the
-- expression to be differentiated. In this situation, the "type tag" of the
-- datum is the algebraic operator symbol (such as '+') and the operation being
-- performed is `deriv`. We can transform this program into data-directed style
-- by rewriting the basic derivative procedure as:

deriv :: Expr -> String -> Expr
deriv (Const _) _   = Const 0
deriv (Var x)   var = Const $ if sameVar x var then 1 else 0
deriv expr      var = get "deriv" (operator expr) (operands expr) var
  where
    get :: String
        -> Map String ([Expr] -> String -> Expr)
        -> ([Expr] -> String -> Expr)
    get = flip (!)

sameVar :: String -> String -> Bool
sameVar = (==)

operator :: Expr -> Map String ([Expr] -> String -> Expr)
operator (BinaryExpr op _ _) = getImplementations op
operator _                   = empty

operands :: Expr -> [Expr]
operands _ = []

-- | a. Explain what was done about. Why can't we assimilate the pattern match
-- on the data constructors `Const` and `Var` into the data-directed dispatch?
--
-- `Const` and `Var` and data constructors, they're not operators so we can't
-- tag dispatch. Note than more generally, the definition of `==` is overloaded
-- so Haskell performs "some kind of" dispatch here...

-- | b. Write the procedures for derivatives of sums and products, and the
-- auxiliary code required to install them into this data-directed system.

getImplementations :: BinaryOp -> Map String ([Expr] -> String -> Expr)
getImplementations Add = fromList [("deriv", derivAdd)]
getImplementations Mul = fromList [("deriv", derivMul)]
getImplementations Pow = fromList [("deriv", derivPow)]
getImplementations _   = empty

derivAdd :: [Expr] -> String -> Expr
derivAdd es var = BinaryExpr Add (deriv e1 var) (deriv e2 var)
  where
    e1 = head es
    e2 = es !! 1

derivMul :: [Expr] -> String -> Expr
derivMul es var = BinaryExpr Add (BinaryExpr Mul e1' e2) (BinaryExpr Mul e1 e2')
  where
    e1  = head es
    e2  = es !! 1
    e1' = deriv e1 var
    e2' = deriv e2 var

-- | c. Choose any additional differentiation rule that you like, such as the
-- one for exponents (exercise 2.56), and install it into this data-directed
-- system.
--
-- This exercise doesn't concern itself with the implementation of `put` for
-- installing `deriv` implementations for expression operators. Haskell's
-- language designers intentionally made doing this sort of
-- "static-initialisation" difficult - type classes solve this issue. As such,
-- I'm going to keep the code pure and "install" the implementation above,
-- understanding that this doesn't have the same modularity as the system SICP
-- describes.
derivPow :: [Expr] -> String -> Expr
derivPow es var =
    BinaryExpr Mul
        (BinaryExpr Pow e1 (BinaryExpr Sub e2 (Const 1)))
        (BinaryExpr Add
            (BinaryExpr Mul e2 e1')
            (BinaryExpr Mul (BinaryExpr Mul e1 (UnaryExpr Ln e1)) e2'))
  where
    e1  = head es
    e2  = es !! 1
    e1' = deriv e1 var
    e2' = deriv e2 var

-- | d. In this simple algebraic maniplutator the type of an expression is the
-- algebraic operator that binds it together. Suppose, however, that we indexed
-- the procedures the opposite way, so that the dispatch line in `deriv`
-- looked like
-- @
-- (get (operator expr) "deriv") (operands expr) var
-- @
-- . What corresponding changes to the derivative system are required?
--
-- Simply implement `get` as `(!)`
