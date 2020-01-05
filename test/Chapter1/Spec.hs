module Chapter1.Spec (chapter1_tests) where
    import Test.HUnit
    import Chapter1.TExercise1
    import Chapter1.TExercise3

    chapter1_tests :: Test
    chapter1_tests = TestList[chapter1Exercise1Tests, chapter1Exercise3Tests]