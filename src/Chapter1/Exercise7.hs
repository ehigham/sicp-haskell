module Chapter1.Exercise7 (sqrt', goodEnough) where
    import Chapter1.Utilities (improve, halve)
-- | The `goodEnough` test used in computing square-roots will not be very
-- | effective for finding the square-roots of very small numbers. Also, in
-- | real computers, arithmetic operations are almost always performed with
-- | limited precision. This makes our test inadequate for very large numbers.
-- | Explain these statements, with examples showing how the test fails for very
-- | small and large numbers.

-- | When attempting to sqrt small numbers, the previous `goodEnough` test
-- | will fail as the value of x approaches the threshold value. See
-- | Chapter1.TExercise7.tSmallNumbers.
-- | When attempting to sqrt large numbers, the previous `goodEnough` test will
-- | fail as the threshold value cannot be represented, instead it will be
-- | rounded down to zero. See Chapter1.TExercise7.tLargeNumbers.

-- | An alternative strategy for implementing `goodEnough` is to watch how
-- | `guess` changes from one iteration to the next and to stop when the change
-- | is a very small fraction of of the guess. Design a square-root procedure
-- | that uses this kind of test. Does this work better for small and large
-- | numbers?

-- | See TExercise7, demonstrating the improved behaviour of goodEnough for
-- | small and large values.

    sqrt' :: (Real x, Fractional x) => x -> x
    sqrt' x = go x (halve x)
      where
          go old new | goodEnough old new = new
                     | otherwise          = go new (improve new x)

    goodEnough :: (Real x, Fractional x) => x -> x -> Bool
    goodEnough old new = abs (1.0 - old / new) < 0.001
