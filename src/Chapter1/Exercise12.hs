module Chapter1.Exercise12 (pascal) where
-- | The following pattern of numbers is called "Pascal's triangle"
--
--             1
--           1   1
--         1   2   1
--       1   3   3   1
--     1   4   6   4   1
--    ...
--
-- The numbers at the edge of the triangle are all 1 and each number inside
-- the triangle is the sum of the two numbers above it. Write a procedure
-- that computes elements of Pascal's triange by means of a recursive process.
pascal :: Int -> Int -> Int
pascal m n = if (n == 0) || (n == m)
    then 1
    else pascal (m - 1) (n - 1) + pascal (m - 1) n
