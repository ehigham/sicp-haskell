module Chapter2.Exercise1 (RatNum, mkRat) where
    import Chapter1.Exercise20 (gcd)
    import Control.Monad (liftM2)
    import Prelude hiding (gcd)
-- | Define a better version of `mkRat` that handles both positive and
-- | negative arguments. `mkRat` should normalize the sign so that if the
-- | rational number is positive, both the numerator ands denominator are
-- | positive and if the number is negative, only the numerator is negative.

    data RatNum = RatNum { numer :: Integer, denom :: Integer }

    mkRat :: Integer -> Integer -> RatNum
    mkRat n d = simplify (n * signum d) (abs d)
      where
        simplify x y = let z = gcd x y in RatNum (x `quot` z) (y `quot` z)

    instance Num RatNum where
        x + y = mkRat n d
          where
            n = numer x * denom y + numer y * denom x
            d = denom x * denom y

        x * y = mkRat n d
          where
            n = numer x * numer y
            d = denom x * denom y

        abs = liftM2 mkRat (abs . numer) denom

        negate = liftM2 mkRat (negate . numer) denom

        signum = fromInteger . signum . numer

        fromInteger x = RatNum x 1

    instance Eq RatNum where
        x == y = numer x == numer y && denom x == denom y

    instance Show RatNum where
        show x = show (numer x) ++ "/" ++ show (denom x)

