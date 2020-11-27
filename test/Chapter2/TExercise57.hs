module Chapter2.TExercise57 (tests) where

import Chapter2.Exercise57 (parseExpr)
import Chapter2.Symbolic (BinaryOp(..), Expr(..))
import Test.HUnit

tests :: Test
tests = TestList [
    TestLabel "Constant"       $ parseExpr "0"       ~=? Just (Const 0),
    TestLabel "Var"            $ parseExpr "x"       ~=? Just (Var "x"),
    TestLabel "Addition"       $ parseExpr "(+ x y z)" ~=? Just (BinaryExpr Add (BinaryExpr Add (Var "x") (Var "y")) (Var "z")),
    TestLabel "Substraction"   $ parseExpr "(- x y z)" ~=? Just (BinaryExpr Sub (BinaryExpr Sub (Var "x") (Var "y")) (Var "z")),
    TestLabel "Multiplication" $ parseExpr "(* x y z)" ~=? Just (BinaryExpr Mul (BinaryExpr Mul (Var "x") (Var "y")) (Var "z")),
    TestLabel "Division"       $ parseExpr "(/ x y z)" ~=? Just (BinaryExpr Div (BinaryExpr Div (Var "x") (Var "y")) (Var "z")),
    TestLabel "Exponentiation" $ parseExpr "(^ x y z)" ~=? Just (BinaryExpr Pow (BinaryExpr Pow (Var "x") (Var "y")) (Var "z")),
    TestLabel "Compound Expr"  $ parseExpr "(* (+ x y) (/ c (^ d e)))"
        ~=? Just (BinaryExpr
                    Mul
                    (BinaryExpr Add (Var "x") (Var "y"))
                    (BinaryExpr
                        Div
                        (Var "c")
                        (BinaryExpr Pow (Var "d") (Var "e"))))
    ]
