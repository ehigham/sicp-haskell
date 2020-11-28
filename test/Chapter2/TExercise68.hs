module Chapter2.TExercise68 (tests) where

import Chapter2.Exercise67 (decode, sampleTree)
import Chapter2.Exercise68 (encode)
import Test.HUnit

tests :: Test
tests = TestCase $ assertEqual
    "encoding the decoded message with the sample HuffmanTree"
    msg
    (encode sampleTree (decode sampleTree msg))
  where
    msg = "0110010101110"
