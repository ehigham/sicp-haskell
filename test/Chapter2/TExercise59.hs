module Chapter2.TExercise59 (tests) where

import Chapter2.Exercise59 (UnorderedList)
import Chapter2.Set
import Test.HUnit

set :: [Int] -> UnorderedList Int
set = fromList

emptyset :: UnorderedList Int
emptyset = mempty

tests :: Test
tests = TestList [
    TestLabel "Adjoin"       $ TestList [
        TestLabel "Empty-Set"    $ set [1]    ~=? adjoin 1 emptyset,
        TestLabel "Not-Contains" $ set [1..5] ~=? adjoin 1 (set [2..5]),
        TestLabel "Contains"     $ set [1..5] ~=? adjoin 1 (set [1..5])
        ],
    TestLabel "isElem"       $ TestList [
        TestCase (assertBool "Empty-Set"    (not $ isElem 1 emptyset)),
        TestCase (assertBool "Not-Contains" (isElem 1 (set [1..5]))),
        TestCase (assertBool "Contains"     (not $ isElem 1 emptyset))
        ],
    TestLabel "Intersection" $ TestList [
        emptyset   ~=? intersect emptyset (set [1..10]),
        emptyset   ~=? intersect (set [1..10]) emptyset,
        set [3, 5] ~=? intersect (set [1..5]) (set [3, 5..9])
        ],
    TestLabel "Union"        $ TestList [
        let x = set [1..10] in x ~=? union x emptyset,
        let x = set [1..10] in x ~=? union emptyset x,
        set [1..10] ~=? union (set [2,4..10]) (set [1,3..9])
        ]
    ]


