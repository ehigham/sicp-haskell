module Chapter2.Spec (chapter2_tests) where
    import Test.HUnit
    import Chapter2.TExercise1
    import Chapter2.TExercise2
    import Chapter2.TExercise3
    import Chapter2.TExercise4
    import Chapter2.TExercise5
    import Chapter2.TExercise6
    import Chapter2.TExercise7

    chapter2_tests :: Test
    chapter2_tests = TestList[
        Chapter2.TExercise1.tests,
        Chapter2.TExercise2.tests,
        Chapter2.TExercise3.tests,
        Chapter2.TExercise4.tests,
        Chapter2.TExercise5.tests,
        Chapter2.TExercise6.tests,
        Chapter2.TExercise7.tests]
