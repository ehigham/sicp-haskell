module Chapter2.TExercise29 (tests) where
    import Chapter2.Exercise29
    import Test.HUnit

    tests :: Test
    tests = TestList [testTotalWeight]

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

    mkWeight = Branch 1 . Left
    mkMobile x y = Mobile (mkWeight x) (mkWeight y)
    mkSubMobile = ((Branch 1 . Right) .) . mkMobile
