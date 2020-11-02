module Chapter2.Exercise19 (usCoins, ukCoins, cc) where
-- | Consider the change-counting program of section 1.2.2. It would be nice
-- | to be able to easily change the currency used by the program so that we
-- | could compute the number of ways to change a British pound, for example.
-- | As the program is written, the knowledge of the currency is distributed
-- | partly into the procedure `firstDenomination` and partly into the procedure
-- | `countChange` (which knows that there are five kinds of U.S. coins). It
-- | would be nicer to be able to supply a list of coins to be used for making
-- | change.
-- | We want to rewrite the procedure `cc` so that its first argument is a
-- | list of the values of the coins to use rather than an integer specifying
-- | which coins to use. We could then have lists that defined each kind of
-- | currency:

    usCoins, ukCoins :: [Int]
    usCoins = [50, 25, 10, 5, 1]
    ukCoins = [100, 50, 20, 10, 5, 2, 1]

-- | We could then call `cc` as follows:
-- >>> cc usCoins 100
-- 292

    cc :: [Int] -> Int -> Int
    cc coins amount | amount == 0                = 1
                    | amount < 0 || noMore coins = 0
                    | otherwise = cc (exceptFirstDenomination coins) amount
                        + cc coins (amount - firstDenomination coins)

-- | Define the procedures `firstDenomination`, `exceptFirstDenomination` and
-- | `noMore` in terms of the primitive operations on list structures.

    firstDenomination :: [a] -> a
    firstDenomination = head

    exceptFirstDenomination :: [a] -> [a]
    exceptFirstDenomination = tail

    noMore :: [a] -> Bool
    noMore = null

-- | Does the order of the list `coins` affect the answer produced by `cc`?
-- | Why or why not?
-- | The order of the coins does not affect the answer as all combinations are
-- | explored.
