module Chapter1.Exercise25 where
    import Chapter1.Exercise16 (fastExpt')
-- | Alyssa P. Hacker complains that we went to a lot of extra work in writing
-- | `expmod`. After all, she says, since we already know how to compute
-- | exponentials, we could have simply written:

    expmod :: (Integral n) => n -> n -> n -> n
    expmod base exp = rem (fastExpt' base exp)

-- | Is she correct? Would her procedure serve as well for our fast prime
-- | tester? Explain.
-- |
-- | This version of `expmod` computes very large temporary values.  If our
-- | system has a mechanism to handle arbitrary-precision arithmetic (e.g.
-- | Haskell's `Integer` type), then this version of `expmod` would work.
-- | That said it may take considerably longer as arithmetic with arbitrarily
-- | long numbers is computationally more expensive.
