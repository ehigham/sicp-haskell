module Chapter2.Exercise23 (forEach) where
-- | The procedure `forEach` is similar to `map`. It takes as arguments a
-- | procedure and a list of elements. However, rather than forming a list
-- | of the results, `forEach` just applies the procedure to each of the
-- | elements in turn, from left to right. The values returned by applying the
-- | procedure to the elements are not used at all - `forEach` is used with
-- | procedures that perform an action, such as printing. For example:
-- >>> forEach (putStrLn . show) ([57, 321, 88] :: [Integer])
-- 57
-- 321
-- 88
-- Give an implementation for `forEach`.

    forEach :: (Monad m) => (a -> m b) -> [a] -> m ()
    forEach _ [] = return ()
    forEach f (x:xs) = (f x) >> forEach f xs
