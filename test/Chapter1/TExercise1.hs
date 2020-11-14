module Chapter1.TExercise1 (tests) where
import Test.HUnit

tests :: Test
tests = TestCase (assertEqual "add [1, 2]" (3 :: Int) (sum [1, 2]))
