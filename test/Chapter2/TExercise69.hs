module Chapter2.TExercise69 (tests) where

import Chapter2.Exercise67 (sampleTree)
import Chapter2.Exercise69 (huffman)
import Test.HUnit

tests :: Test
tests = TestCase $ assertEqual
    "generating huffman from book with `huffman`"
    sampleTree
    (huffman [(4, 'A'), (2, 'B'), (1, 'D'), (1, 'C')])

