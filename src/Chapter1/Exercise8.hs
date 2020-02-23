module Chapter1.Exercise8 where
    import Chapter1.Exercise7 (goodEnough)
    import Chapter1.Utilities (square)
-- | Newton's method for cube roots is based on the fact that if y is an
-- | approximation for the cube root of x, then a better approximation is
-- | given by the value
    improve :: (Real x, Fractional x) => x -> x -> x
    improve x y = third $ x / square y + 2 * y

-- | Use this formula to implement a cube-root procedure analogous to the 
-- | square-root procedure. (In Section 1.3.4 we will see how to implement
-- | Newton's method in general as an abstraction of these square-root and
-- | cube-root procedures).

    cbrt' :: (Real x, Fractional x) => x -> x
    cbrt' x = cbrtIter x (third x) x

    cbrtIter :: (Real x, Fractional x) => x -> x -> x-> x
    cbrtIter old new x 
        | goodEnough old new = new
        | otherwise = cbrtIter new (improve x new) x

    third :: (Real x, Fractional x) => x -> x
    third = (/ 3)
