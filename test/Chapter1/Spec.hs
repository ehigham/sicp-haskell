module Chapter1.Spec (chapter1_tests) where
    import Test.HUnit
    import Chapter1.TExercise1

    chapter1_tests :: Test
    chapter1_tests = TestList[chapter1_exercise1_tests]