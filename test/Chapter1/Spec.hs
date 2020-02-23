module Chapter1.Spec (chapter1_tests) where
    import Test.HUnit
    import Chapter1.TExercise1
    import Chapter1.TExercise3
    import Chapter1.TExercise4
    import Chapter1.TExercise6
    import Chapter1.TExercise7
    import Chapter1.TExercise8

    chapter1_tests :: Test
    chapter1_tests = TestList[
        chapter1Exercise1Tests,
        chapter1Exercise3Tests,
        chapter1Exercise4Tests,
        chapter1Exercise6Tests,
        chapter1Exercise7Tests,
        chapter1Exercise8Tests]