module Chapter2.Exercise29 (
    Mobile (Mobile, left, right),
    Branch (Branch, length, structure),
    totalWeight
) where
    import Control.Applicative (liftA2)
-- | A binary `Mobile` consists of two branches, a left branch and a right
-- | `Branch`. Each branch is a rod of a certain length, from which hangs
-- | either a weight or another binary mobile. We cam represent a binary mobile
-- | using compound data by constructing it from two branches:
    data Mobile = Mobile { left :: Branch
                         , right :: Branch
                         }

-- | A `Branch` is constructed from a `length` (which must be a number) together
-- | with a `structure`, which may be ether a number (represting a simple
-- | weight) or another `Mobile`:
    data Branch = Branch { length :: Integer
                         , structure :: Either Integer Mobile
                         }

-- | a. Write the corresponding selectors `left` and `right` which return the
-- | branches of the `Mobile` and `length` and `branch` which return the
-- | components of the branch
-- complete by definition.

-- | b. Using your selectors, define a procedure `totalWeight` that returns the
-- | total weight of your mobile.
    totalWeight :: Mobile -> Integer
    totalWeight =
        liftA2 (+) (weigh . structure . left) (weigh . structure . right)
      where
        weigh (Left weight) = weight
        weigh (Right mobile) = totalWeight mobile
