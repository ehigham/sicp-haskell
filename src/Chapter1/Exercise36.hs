{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Chapter1.Exercise36 (averageDamp) where
    import Chapter1.Exercise35 (fixedPointM)
    import Chapter1.Utilities (average)
    import Control.Monad (ap, liftM2)
-- | Modify `fixed` so that it prints the sequence of approximations it
-- | generates, using the  `newline` and `display` primitives showin in exercise
-- | 1.22. Then find a solution to x^x = 1,000 by finding a fixed point of
-- | \x -> log 1000 / log x (Use Haskell's primitive log procedure, which
-- | computes natural logarithms). Compare the number of steps this takes with
-- | and without average damping (Note that you cannot start fixedPoint with a
-- | guess of 1 as this would cause division by log 1 = 0).

    printAndReturn :: (Show a) => a -> IO a
    printAndReturn = liftM2 (>>) print return

    f :: Double -> Double
    f = (log 1000 /) . log

    averageDamp :: (Fractional a) => (a -> a) -> (a -> a)
    averageDamp = ap average

    solution, avgDamped :: Double -> IO Double
    solution = fixedPointM (printAndReturn . f)
    avgDamped = fixedPointM (printAndReturn . averageDamp f)

-- >>> solution 5
-- 4.29202967422018
-- 4.741863119908242
-- 4.438204569837609
-- 4.635299887107611
-- 4.50397811613643
-- 4.589989462723705
-- 4.53301150767844
-- 4.570475672855484
-- 4.545720389670642
-- 4.562024936588171
-- 4.551263234080531
-- 4.55835638768598
-- 4.553676852183342
-- 4.55676216434628
-- 4.554727130670954
-- 4.556069054770006
-- 4.555184018843625
-- 4.5557676565438205
-- 4.555382746639082
-- 4.55563658243586
-- 4.555469180245326
-- 4.555579577900997
-- 4.5555067722873686
-- 4.5555547860484085
-- 4.555523121789556
-- 4.555544003742869
-- 4.555530232469306
-- 4.555539314360711
-- 4.555539314360711

-- >>> avgDamped 5
-- 4.64601483711009
-- 4.571611286076025
-- 4.558294317536066
-- 4.556006022881116
-- 4.555615799731297
-- 4.555549342575593
-- 4.555538027101999
-- 4.5555361005218895
-- 4.5555361005218895
