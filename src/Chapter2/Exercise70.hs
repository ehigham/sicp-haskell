{-# OPTIONS_GHC -Wno-unused-imports #-}

module Chapter2.Exercise70 (rock, song) where

import Chapter2.Exercise67 (HuffmanTree, decode)
import Chapter2.Exercise68 (encode)
import Chapter2.Exercise69 (huffman)

-- | The following eight-symbol alphabet with associated relative frequencies
-- was designed to efficiently encode the lyrics of the 1950s rock songs (note
-- that symbols of an alphabet need not be individual letters).
--
--          ┌────────────┬───────────┬────────────┬───────────┐
--          │ Symbol     │ Frequency │ Symbol     │ Frequency │
--          ├────────────┼───────────┼────────────┼───────────┤
--          │ A          │ 2         │ NA         │ 16        │
--          ├────────────┼───────────┼────────────┼───────────┤
--          │ BOOM       │ 1         │ SHA        │ 3         │
--          ├────────────┼───────────┼────────────┼───────────┤
--          │ GET        │ 2         │ YIP        │ 9         │
--          ├────────────┼───────────┼────────────┼───────────┤
--          │ JOB        │ 2         │ WAH        │ 1         │
--          └────────────┴───────────┴────────────┴───────────┘
--
-- Use `huffman` (exercise 2.69) to generate a corresponding Huffman tree.
rock :: HuffmanTree String
rock = huffman [(1,"BOOM"), (1,"WAH"), (2,"GET"), (2,"JOB"),
                (2,"A"),    (3,"SHA"), (9,"YIP"), (16,"NA")]

-- | Use `encode` (exercise 2.68) to encode the following message
song :: [String]
song = words =<< ["GET A JOB",
                  "SHA NA NA NA NA NA NA NA NA",
                  "GET A JOB",
                  "SHA NA NA NA NA NA NA NA NA",
                  "WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP",
                  "SHA BOOM"]
-- >>> encode rock song
-- "111101101111111110000000001111011011111111100000000011000101010101010101010111011001"
-- >>> length $ encode rock song
-- 84

-- | What is the smallest number of bits that would be needed to encode this
-- song if we used a fixed-length code for the eight-symbol alphabet.
--
-- To encode 8 symbols, we need ceil (log2 8) bits = 3
-- >>> 3 * length song
-- 108

