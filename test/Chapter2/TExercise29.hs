{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Chapter2.TExercise29 (tests) where
    import Chapter2.Exercise29
    import Test.HUnit

    tests :: Test
    tests = TestList [testTotalWeight, testBalanced]

    mkWeight = Branch 1 . Left
    mkMobile x y = Mobile (mkWeight x) (mkWeight y)
    mkSubMobile = ((Branch 1 . Right) .) . mkMobile

    testTotalWeight :: Test
    testTotalWeight = TestList [
        TestLabel "flat" $ TestList [
            2 ~=? (totalWeight $ mkMobile 1 1),
            3 ~=? (totalWeight $ mkMobile 1 2),
            4 ~=? (totalWeight $ mkMobile 2 2)],
        TestLabel "tree" $ TestList [
            3 ~=? (totalWeight $ (Mobile (mkWeight 1) (mkSubMobile 1 1))),
            10 ~=? (totalWeight $ (Mobile (mkSubMobile 1 2) (mkSubMobile 3 4)))]
            ]

    testBalanced :: Test
    testBalanced = TestList [
        (balanced $ mkMobile 1 1) ~? "flat mobile was not balanced",
        (not . balanced $ mkMobile 1 2) ~? "weight should effect torque",
        (not . balanced $ Mobile (mkWeight 1) (Branch 2 (Left 1))) ~? "length should effect torque",
        (balanced $ Mobile (mkWeight 2) (Branch 2 (Left 1))) ~? "testing weight * distance",
        (balanced $ Mobile (mkSubMobile 1 1) (mkSubMobile 1 1)) ~? "balanced sub-mobiles",
        (not . balanced $ Mobile (mkSubMobile 1 2) (mkSubMobile 1 1)) ~? "un-balanced sub-mobiles",
        (balanced $ Mobile (mkWeight 2) (mkSubMobile 1 1)) ~? "weight and a mobile"
        ]
