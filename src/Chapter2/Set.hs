
module Chapter2.Set ( Set(adjoin, isElem, intersect, union) ) where

class Set t where

    -- | Adjoin the element to the set
    adjoin :: (Ord a) => a -> t a -> t a

    -- | Test if the item is an element of the set
    isElem  :: (Ord a) => a -> t a -> Bool

    -- | Set-intersection
    -- Returns the set of elements that appear in both sets
    intersect :: (Ord a) => t a -> t a -> t a

    -- | Set-union of two sets
    -- Returns the set containing all elements from both sets
    union :: (Ord a) => t a -> t a -> t a
