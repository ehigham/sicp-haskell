
module Chapter2.Set
    (
        Set,
        adjoin,
        isElem,
        fromList,
        intersect,
        singleton,
        union
    ) where

class Set t where

    -- | Adjoin the element to the set
    adjoin :: (Ord a) => a -> t a -> t a

    -- | Test if the item is an element of the set
    isElem :: (Ord a) => a -> t a -> Bool

    -- | Create a Set whose members are adjoined from the list of elements
    fromList :: (Ord a) => [a] -> t a

    -- | Set-intersection
    -- Returns the set of elements that appear in both sets
    intersect :: (Ord a) => t a -> t a -> t a

    -- | Set-union of two sets
    -- Returns the set containing all elements from both sets
    union :: (Ord a) => t a -> t a -> t a

singleton :: (Ord a, Set t) => a -> t a
singleton x = fromList [x]
