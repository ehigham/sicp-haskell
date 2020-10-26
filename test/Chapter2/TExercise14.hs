module Chapter2.TExercise14 (tests) where
    import Chapter2.Exercise7 (mkInterval)
    import Chapter2.Exercise14 (par1, par2)
    import TestTools ((~/=?))
    import Test.HUnit

    testIdentity :: Test
    testIdentity = TestLabel "1 /= x/x" $ one ~/=? (x / x)
      where
        one = mkInterval 1 1
        x = mkInterval 2 4

    testParNotSame :: Test
    testParNotSame =
        TestLabel "par1 /= par2" $ go (mkInterval 2 8) (mkInterval 2 8)
      where
        go x y = (par1 x y) ~/=? (par2 x y)

    tests :: Test
    tests = TestList [testIdentity, testParNotSame]
