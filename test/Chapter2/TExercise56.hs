{-# LANGUAGE QuasiQuotes #-}
module Chapter2.TExercise56 (tests) where

import Chapter2.Symbolic (expr, deriv)
import Test.HUnit

tests :: Test
tests = TestList [
    TestLabel "Constant" $ [expr|0|] ~=? deriv [expr|1|] [expr|x|],
    TestLabel "Same var" $ [expr|1|] ~=? deriv [expr|x|] [expr|x|],
    TestLabel "Different var" $ [expr|0|] ~=? deriv [expr|z|] [expr|x|],
    TestLabel "Addition" $ TestList [
        [expr|1 + 1|] ~=? deriv [expr|x + x|] [expr|x|],
        [expr|1 + 0|] ~=? deriv [expr|x + 1|] [expr|x|],
        [expr|0 + 0|] ~=? deriv [expr|y + y|] [expr|x|]
        ],
    TestLabel "Subtraction" $ TestList [
        [expr|1 - 1|] ~=? deriv [expr|x - x|] [expr|x|],
        [expr|1 - 0|] ~=? deriv [expr|x - 1|] [expr|x|],
        [expr|0 - 0|] ~=? deriv [expr|y - y|] [expr|x|]
        ],
    TestLabel "Multiplication" $ TestList [
        [expr|1 * x + x * 1|] ~=? deriv [expr|x * x|] [expr|x|],
        [expr|1 * y + x * 0|] ~=? deriv [expr|x * y|] [expr|x|],
        [expr|0 * x + 2 * 1|] ~=? deriv [expr|2 * x|] [expr|x|]
        ],
    TestLabel "Division" $ TestList [
        [expr|(0 * x - 1 * 1)/(x^2)|] ~=? deriv [expr|1 / x|] [expr|x|],
        [expr|(0 * y - 1 * 0)/(y^2)|] ~=? deriv [expr|1 / y|] [expr|x|]
        ],
    TestLabel "Exponentiation" $ TestList [
        [expr|x^(2-1)*(2*1 + x*(ln x)*0)|] ~=? deriv [expr|x^2|] [expr|x|],
        [expr|x^(x-1)*(x*1 + x*(ln x)*1)|] ~=? deriv [expr|x^x|] [expr|x|]
        ],
    TestLabel "Natural Log" $ TestList [
        [expr|1/x * 1|] ~=? deriv [expr|ln x|] [expr|x|],
        [expr|1/x * 0|] ~=? deriv [expr|ln x|] [expr|y|]
        ]
    ]
