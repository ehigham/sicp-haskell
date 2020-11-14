module Chapter1.Exercise38 (calculateExp) where
import Chapter1.Exercise37 (contFrac)

-- | In 1737, the Swiss mathematician Leonhard Euler published a memoir
-- "De Fractionibus Continuis", which included included a continued fraction
-- expansion for `e` - 2 where `e` is the base of the natural logarithms. In
-- this faction, the Ni are all 1 and the Di are successively
-- 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ... . Write a program that uses your
-- `contFrac` procedure from exercise 1.37 to approximate `e`, based on
-- Euler's expansion.
calculateExp :: Int -> Double
calculateExp = (2 +) . contFrac (const 1) kthDTerm
  where
    kthDTerm k = if k `mod` 3 == 2
                    then (fromIntegral k + 1) / 1.5
                    else 1

-- >>> calculateExp 8
-- 2.718279569892473
-- | k = 8 -> accurate to 4 decimal places!
