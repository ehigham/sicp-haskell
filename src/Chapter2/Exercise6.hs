{-# LANGUAGE Rank2Types #-}
module Chapter2.Exercise6 (ChurchNumeral, church, fromChurch) where
    import Control.Monad (liftM2)
    import Data.Function (on)
-- | In case prepresenting pairs as procedures wasn't mind-boggling enough,
-- | consider that, in a language that can manipulate procedures, we can get
-- | by without numbers (at least insofar as nonnegative integers are concerned)
-- | by implementing 0 and 1 as the operation of adding one as
-- @
--      zero = \f x -> x
--    addone = \n -> \f x -> f (n f x)
-- @
-- | This representation is known as `Church numerals`, after its inventor,
-- | Alonzo Church, the logician who invented the lambda calculus.
-- | Define one and two directly (not in terms of `zero` and `addone`) (Hint:
-- | use the substitution rule to evaluate `addone zero`). Give a direct
-- | definition of the addition procedure `+` (not in terms of repeated
-- | application off `addone`)

    newtype ChurchNumeral =
        ChurchNumeral { apply :: forall a. (a -> a) -> a -> a }

    zero, one :: ChurchNumeral
    zero = ChurchNumeral $ const id
    one =  ChurchNumeral $ ($)

    church :: (Integral n) =>  n -> ChurchNumeral
    church n = ChurchNumeral $ \f x -> iterate f x !! fromIntegral n

    fromChurch :: ChurchNumeral -> Integer
    fromChurch n = apply n (+1) 0

    instance Show ChurchNumeral where
        show = show . fromChurch

    instance Eq ChurchNumeral where
        (==) = (==) `on` fromChurch

    instance Ord ChurchNumeral where
        (<=) = (<=) `on` fromChurch

    instance Num ChurchNumeral where
        a + b = ChurchNumeral $ liftM2 (.) (apply a) (apply b)
        a * b = ChurchNumeral $ (apply a) . (apply b)
        abs = id
        signum = const one
        fromInteger = church
        negate n = if n == zero
                    then zero
                    else error "Church numerals cannot be negative"

    instance Enum ChurchNumeral where
        toEnum = church
        fromEnum = fromInteger . fromChurch
