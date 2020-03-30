module Chapter1.Spec (chapter1_tests) where
    import Test.HUnit
    import Chapter1.TExercise1
    import Chapter1.TExercise3
    import Chapter1.TExercise4
    import Chapter1.TExercise6
    import Chapter1.TExercise7
    import Chapter1.TExercise8
    import Chapter1.TExercise9
    import Chapter1.TExercise11
    import Chapter1.TExercise12
    import Chapter1.TExercise14
    import Chapter1.TExercise15
    import Chapter1.TExercise16
    import Chapter1.TExercise17
    import Chapter1.TExercise18
    import Chapter1.TExercise19
    import Chapter1.TExercise21
    import Chapter1.TExercise24
    import Chapter1.TExercise27
    import Chapter1.TExercise28
    import Chapter1.TExercise29
    import Chapter1.TExercise30
    import Chapter1.TExercise31

    chapter1_tests :: Test
    chapter1_tests = TestList[
        chapter1Exercise1Tests,
        chapter1Exercise3Tests,
        chapter1Exercise4Tests,
        chapter1Exercise6Tests,
        chapter1Exercise7Tests,
        Chapter1.TExercise8.tests,
        Chapter1.TExercise9.tests,
        Chapter1.TExercise11.tests,
        Chapter1.TExercise12.tests,
        Chapter1.TExercise14.tests,
        Chapter1.TExercise15.tests,
        Chapter1.TExercise16.tests,
        Chapter1.TExercise17.tests,
        Chapter1.TExercise18.tests,
        Chapter1.TExercise19.tests,
        Chapter1.TExercise21.tests,
        Chapter1.TExercise24.tests,
        Chapter1.TExercise27.tests,
        Chapter1.TExercise28.tests,
        Chapter1.TExercise29.tests,
        Chapter1.TExercise30.tests,
        Chapter1.TExercise31.tests]