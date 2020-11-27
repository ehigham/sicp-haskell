module Chapter2.TExercise58 (tests) where


import Chapter2.Exercise58 (parseExpr)
import Chapter2.Symbolic (BinaryOp(..), Expr(..))
import Test.HUnit

tests :: Test
tests = TestList [
    TestLabel "Constant"       $ parseExpr "0"       ~=? Just (Const 0),
    TestLabel "Var"            $ parseExpr "x"       ~=? Just (Var "x"),
    TestLabel "Addition"       $ parseExpr "(x + y)" ~=? Just (BinaryExpr Add (Var "x") (Var "y")),
    TestLabel "Substraction"   $ parseExpr "(x - y)" ~=? Just (BinaryExpr Sub (Var "x") (Var "y")),
    TestLabel "Multiplication" $ parseExpr "(x * y)" ~=? Just (BinaryExpr Mul (Var "x") (Var "y")),
    TestLabel "Substraction"   $ parseExpr "(x / y)" ~=? Just (BinaryExpr Div (Var "x") (Var "y")),
    TestLabel "Exponentiation" $ parseExpr "(x ^ y)" ~=? Just (BinaryExpr Pow (Var "x") (Var "y")),
    TestLabel "Compound Expr"  $ parseExpr "((x + y) * (c / (d ^ e)))"
        ~=? Just (BinaryExpr
                    Mul
                    (BinaryExpr Add (Var "x") (Var "y"))
                    (BinaryExpr
                        Div
                        (Var "c")
                        (BinaryExpr Pow (Var "d") (Var "e"))))
    ]
