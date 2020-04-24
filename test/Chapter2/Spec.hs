module Chapter2.Spec (chapter2_tests) where
    import Test.HUnit
    import Chapter2.TExercise1

    chapter2_tests :: Test
    chapter2_tests = TestList[
        Chapter2.TExercise1.tests]
