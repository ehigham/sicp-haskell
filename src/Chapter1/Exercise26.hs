module Chapter1.Exercise26 where
    import Chapter1.Utilities (halveI)
-- | Louis Reasoner is having great difficulty doing exercise 1.24. His
-- | `fastPrime` test seems to be slower than his `isPrime` test. Louis calls
-- | his friend Eva Lu Ator over to help. When they examine Louis' code, they
-- | find that he has re-written the `expmod` procedure to use an explicit
-- | multiplication, rather than calling square:

    expmod :: (Integral n) => n -> n -> n -> n
    expmod base exp m
        | exp == 0  = 1
        | even exp  = (expmod base (halveI exp) m) *
                      (expmod base (halveI exp) m) `mod` m
        | otherwise = (base * expmod base (pred exp) m) `mod` m

-- | "I don't see what difference that could make," says Louis. "I do." says
-- | Eva. "By writing the procedure like that, you have transformed the
-- | Theta(log n) process into a Theta(n) process." Explain.
--
-- | Intead of linear recursion, this `expmod` process generates tree recursion
-- | whose execution time grows exponentially with the depth of the tree, which
-- | is `log n`. Thus, the runtime of this `expmod` scales Theta(n).
