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
import Chapter2.TExercise28
import Chapter2.TExercise29
import Chapter2.TExercise32
import Chapter2.TExercise33
import Chapter2.TExercise34
import Chapter2.TExercise36
import Chapter2.TExercise37
import Chapter2.TExercise39
import Chapter2.TExercise41
import Chapter2.TExercise56
import Chapter2.TExercise57
import Chapter2.TExercise58
import Chapter2.TExercise59
import Chapter2.TExercise61
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
    TestLabel "Exercise27" Chapter2.TExercise27.tests,
    TestLabel "Exercise27" Chapter2.TExercise28.tests,
    TestLabel "Exercise29" Chapter2.TExercise29.tests,
    TestLabel "Exercise32" Chapter2.TExercise32.tests,
    TestLabel "Exercise33" Chapter2.TExercise33.tests,
    TestLabel "Exercise34" Chapter2.TExercise34.tests,
    TestLabel "Exercise36" Chapter2.TExercise36.tests,
    TestLabel "Exercise37" Chapter2.TExercise37.tests,
    TestLabel "Exercise39" Chapter2.TExercise39.tests,
    TestLabel "Exercise41" Chapter2.TExercise41.tests,
    TestLabel "Exercise56" Chapter2.TExercise56.tests,
    TestLabel "Exercise57" Chapter2.TExercise57.tests,
    TestLabel "Exercise58" Chapter2.TExercise58.tests,
    TestLabel "Exercise59" Chapter2.TExercise59.tests,
    TestLabel "Exercise61" Chapter2.TExercise61.tests
    ]
