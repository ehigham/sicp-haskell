{-# LANGUAGE QuasiQuotes #-}
module Chapter2.TExercise56 (tests) where

import Chapter2.Symbolic (expr, deriv)
import Test.HUnit

tests :: Test
tests = TestList [
    TestLabel "Constant" $ [expr|0|] ~=? deriv [expr|1|] "x",
    TestLabel "Same var" $ [expr|1|] ~=? deriv [expr|x|] "x",
    TestLabel "Different var" $ [expr|0|] ~=? deriv [expr|z|] "x",
    TestLabel "Addition" $ TestList [
        [expr|1 + 1|] ~=? deriv [expr|x + x|] "x",
        [expr|1 + 0|] ~=? deriv [expr|x + 1|] "x",
        [expr|0 + 0|] ~=? deriv [expr|y + y|] "x"
        ],
    TestLabel "Subtraction" $ TestList [
        [expr|1 - 1|] ~=? deriv [expr|x - x|] "x",
        [expr|1 - 0|] ~=? deriv [expr|x - 1|] "x",
        [expr|0 - 0|] ~=? deriv [expr|y - y|] "x"
        ],
    TestLabel "Multiplication" $ TestList [
        [expr|1 * x + x * 1|] ~=? deriv [expr|x * x|] "x",
        [expr|1 * y + x * 0|] ~=? deriv [expr|x * y|] "x",
        [expr|0 * x + 2 * 1|] ~=? deriv [expr|2 * x|] "x"
        ],
    TestLabel "Division" $ TestList [
        [expr|(0 * x - 1 * 1)/(x^2)|] ~=? deriv [expr|1 / x|] "x",
        [expr|(0 * y - 1 * 0)/(y^2)|] ~=? deriv [expr|1 / y|] "x"
        ],
    TestLabel "Exponentiation" $ TestList [
        [expr|x^(2-1)*(2*1 + x*(ln x)*0)|] ~=? deriv [expr|x^2|] "x",
        [expr|x^(x-1)*(x*1 + x*(ln x)*1)|] ~=? deriv [expr|x^x|] "x"
        ],
    TestLabel "Natural Log" $ TestList [
        [expr|1/x * 1|] ~=? deriv [expr|ln x|] "x",
        [expr|1/x * 0|] ~=? deriv [expr|ln x|] "y"
        ]
    ]
