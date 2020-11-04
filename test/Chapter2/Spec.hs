module Chapter2.Spec (chapter2_tests) where
    import Chapter2.TExercise1
    import Chapter2.TExercise2
    import Chapter2.TExercise3
    import Chapter2.TExercise4
    import Chapter2.TExercise5
    import Chapter2.TExercise6
    import Chapter2.TExercise7
    import Chapter2.TExercise14
    import Chapter2.TExercise17
    import Chapter2.TExercise18
    import Chapter2.TExercise20
    import Chapter2.TExercise27
    import Test.HUnit

    chapter2_tests :: Test
    chapter2_tests = TestList [
        TestLabel "Exercise1" Chapter2.TExercise1.tests,
        TestLabel "Exercise2" Chapter2.TExercise2.tests,
        TestLabel "Exercise3" Chapter2.TExercise3.tests,
        TestLabel "Exercise4" Chapter2.TExercise4.tests,
        TestLabel "Exercise5" Chapter2.TExercise5.tests,
        TestLabel "Exercise6" Chapter2.TExercise6.tests,
        TestLabel "Exercise7" Chapter2.TExercise7.tests,
        TestLabel "Exercise14" Chapter2.TExercise14.tests,
        TestLabel "Exercise17" Chapter2.TExercise17.tests,
        TestLabel "Exercise18" Chapter2.TExercise18.tests,
        TestLabel "Exercise20" Chapter2.TExercise20.tests,
        TestLabel "Exercise27" Chapter2.TExercise27.tests
        ]
