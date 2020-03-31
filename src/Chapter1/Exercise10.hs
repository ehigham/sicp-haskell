{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter1.Exercise10 (ackermann) where
    import Chapter1.Utilities (square)

-- | The following procedure computes a mathematical function called
-- | Ackermann's function.

    ackermann :: Int -> Int -> Int
    ackermann x y | y == 0    = 0
                  | x == 0    = 2 * y
                  | y == 1    = 2
                  | otherwise = ackermann (x - 1) (ackermann x (y - 1))

-- | What are the values of the following expressions?
-- >> ackermann 1 10
-- 1024
-- >> ackermann 2 4
-- 65536
-- >> ackermann 3 3
-- 65536

-- | Consider the following procedures.

    f, g, h, k :: Int -> Int
    f = ackermann 0
    g = ackermann 1
    h = ackermann 2
    k = (* 5) . square

-- | Give concise mathematical definitions for the functional computed by the
-- | procedures `f`, `g`, and `h` for positive integer inputs. For example,
-- | `(k n)` computes 5*n^2

-- | `f n` computes 2*n
-- | `g n` computes 0 when n == 0, n^2 for n > 0
-- | `h n` computes 0 when n == 0, 2 for n == 1, 2^2^… (n-1 times) for n > 1