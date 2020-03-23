module Chapter1.Exercise19 (fib) where
    import Chapter1.Utilities (square)
-- | There is a clever algorithm for computing the Fibonacci numbers in a
-- | logarithmic number of steps. Recall the transformation of the state
-- | variables `a` and `b` in the `fibIter` process of section 1.2.2:
-- @
--      a <- a + b
--      b <- a
-- @
-- | Call this transformation `T` and observe that applying `T` over and over
-- | again `n` times, starting with 0 and 1, produces the pair `fib (n + 1)` and
-- | `fib n`. In other words, the Fibonacci numbers ae produced by applying
-- | `T^n`, the `n`th power of the transformation `T`, starting with the pair
-- | (0, 1). Now consider `T` to the special case pf `p` = 0 and and `q` = 1
-- | in the family of transformations `T_{pq}`, where `T_{pq}` transforms the
-- | pair (`a`, `b`) according to
-- @
--      a <- b.q + a.(p + q)
--      b <- b.p + a.q
-- @
-- | Show that if we apply such a transformation `T_{pq}` twice, the effect is
-- | the same as using a single transformation `T_{p'q'}` of the same form and
-- | compute `p'` and `q'` in terms of `p` and `q`. This gives us an explicit
-- | way to square these transformations and thus we can compute `T^n` using
-- | successive squaring, as in the `fastExpt` procedure.
-- @
--      a' <- b.q + a.(p + q)
--      b' <- b.p + a.q
--
--      a'' <- b'.q + a'.(p + q)
--              = (b.p + a.q).q + (b.q + a.(p + q)).(q + q)
--              = b.p.q + a.q.q + b.q.(p + q) + a.(p + q).(p + q)
--              = b.(2.p.q + q.q) + a.(q.q + (p + q)(p + q))
--              = b.q.(2.p + q) + a.((p.p + q.q) + q.(2.p + q))
--
--      b'' <- b'.p + a'.q =  (b.p + a.q).p + (b.q + a.(p + q)).q
--              = b.p.p + a.p.q + b.q.q + a.q.(p + q)
--              = b.(p.p + q.q) + a.q.(2.p + q)
-- @
-- | By examining the original transformation, we have:
-- @
--      p' = p.p + q.q
--      q' = q.(2.p + q)
-- @
-- | Put this all together to complete the following procedure, which runs in
-- | a logarithmic number of steps:

    fib :: (Integral n) => n -> n
    fib = go 1 0 0 1
        where
            go a b p q count
                | count == 0 = b
                | even count = go a b (p' p q) (q' p q) (count `div` 2)
                | otherwise = go (b*q + a*(p + q)) (b*p + a*q) p q (count - 1)
            p' p q = square p + square q
            q' p q = q * (2 * p + q)