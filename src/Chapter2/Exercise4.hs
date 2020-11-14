{-# LANGUAGE Rank2Types #-}
module Chapter2.Exercise4 (cons, car, cdr, Pair) where
-- | Here is an alternative procedural representation of pairs. For this
-- representation, verify that (car (cons x y)) yeilds x for any objects x
-- and y.
newtype Pair a b = Pair { pick :: forall r. (a -> b -> r) -> r }

cons :: a -> b -> Pair a b
cons x y = Pair $ \m -> m x y

car :: Pair a b -> a
car p = pick p const

-- | What is the corresponding definition for `cdr`? (Hint: To verify that this
--  works, make use of the substitution model of section 1.1.5)
cdr :: Pair a b -> b
cdr p = pick p (const id)

-- | Via the substitution model:
-- car:
-- >>> car (cons x y)
-- car (Pair $ \m -> x y)
-- pick (Pair $ \m -> x y) const
-- (\m -> m x y) (\p _ -> p)
-- (\p _ -> p) x y
-- x
--
-- | cdr
-- >>> cdr (cons x y)
-- cdr (Pair $ \m -> x y)
-- pick (Pair $ \m -> x y) (const id)
-- (\m -> m x y) (\_ q -> q)
-- (\_ q -> q) x y
-- y

instance (Show a, Show b) => Show (Pair a b) where
    show p = "(" ++ show (car p) ++ ", " ++ show (cdr p) ++ ")"

instance (Eq a, Eq b) => Eq (Pair a b) where
    x == y = car x == car y && cdr x == cdr y
