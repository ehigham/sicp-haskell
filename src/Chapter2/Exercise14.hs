module Chapter2.Exercise14 (par1, par2) where
    import Chapter2.Exercise7 (Interval)
-- | After considerable work, Alyssa P. Hacker delivers her finished system.
-- | Several years later, after she has forgotten all about it, she gets a
-- | frenzied call from an irate user, Lem E. Tweakit. It seems that Lem has
-- | noticed that the formula for parallel resistors can be written in two
-- | algebraically equivalent ways:
-- @
--      (R1 * R2) / (R1 + R2)
-- @
-- | and
-- @
--      1 / (1 / R1 + 1 / R2)
-- @
-- | He has written the following two programs, each of which computes the
-- | parallel resistors formula differently:

    par1 :: Interval -> Interval -> Interval
    par1 r1 r2 = (r1 * r2) / (r1 + r2)

    par2 :: Interval -> Interval -> Interval
    par2 r1 r2 = recip (recip r1 + recip r2)

-- | Lem Complains that Alyssa's program gives different answers fot the two
-- | ways of computing. This is a serious complaint.
--
-- | Exercise 2.14
-- | Demonstrate that Lem is right. INvestigate the behaviour of the system on a
-- | variety of arithmetic expressions. Make some intervals `A` and `B`, and use
-- | them in computing the expression `A `div` A` and `A `div` B`. You will get
-- | the most insight by using intervals whose width is a small percentage of
-- | the centre value. Examine the results of the computation in centre-percent
-- | form (see exercise 2.12).

-- See Chapter1.TExercise14
