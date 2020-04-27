module Chapter2.Spec (chapter2_tests) where
    import Test.HUnit
    import Chapter2.TExercise1
    import Chapter2.TExercise2
    import Chapter2.TExercise3
    import Chapter2.TExercise4

    chapter2_tests :: Test
    chapter2_tests = TestList[
        Chapter2.TExercise1.tests,
        Chapter2.TExercise2.tests,
        Chapter2.TExercise3.tests,
        Chapter2.TExercise4.tests]
