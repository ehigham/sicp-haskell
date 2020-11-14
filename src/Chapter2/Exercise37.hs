module Chapter2.Exercise37 (
    Vector,
    Matrix,
    dotProduct,
    dotProductM,
    matmul,
    transpose
) where

import Chapter2.Exercise36 (accumulateN)

-- | Suppose we represent vectors `v = [v_i]` as sequences of numbers, and
-- matrices `m = [m_{i,j}]` as sequences of vectors (the rows of the matrix).
-- For example, the matrix
-- @
-- ⌈1 2 3 4⌉
-- ⎸4 5 6 6⎹
-- ⌊6 7 8 9⌋
-- @
-- is represented as the sequence
-- @
-- [[1,2,3,4], [4,5,6,6], [6,7,8,9]]
-- @
-- With this representation, we can use sequence operations to concisely express
-- basic matric and vector operations. These operations (which are described in
-- any bool on matrix algebra) are the following:

type Vector a = [a]
type Matrix a = [Vector a]

-- | Returns the sum over i, v_i w_i
dotProduct :: (Num a) => Vector a -> Vector a -> a
dotProduct v w = sum $ (*) <$> v <*> w

-- | Retuns the vector t, where t = sum over j, m_{i,j} v_{j}
dotProductM :: (Num a) => Matrix a -> Vector a -> Vector a
dotProductM m v = dotProduct v <$> transpose m

-- | Retuns the matrix p, where p = sum over i,j, m_{i,k} n_{k,sj}
matmul :: (Num a) => Matrix a -> Matrix a -> Matrix a
matmul m n = dotProductM m <$> transpose n

-- | Return the matrix m where m_{i,j} = n_{j,i}
transpose :: Matrix a -> Matrix a
transpose = accumulateN (:) []
