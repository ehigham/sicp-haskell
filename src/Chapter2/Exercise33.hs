module Chapter2.Exercise33 (map', append', length') where

import Control.Applicative ( Alternative(empty, (<|>)) )

-- | Fill in the missing expresions to complete to following definitions
-- of some basic list-manipulation operations as accumulations.
-- Note that some of these will fail for trees as fold is not structure-
-- preserving.
map' :: (Foldable t, Alternative t) => (a -> b) -> t a -> t b
map' f = foldr ((<|>) . pure . f) empty

append' :: (Foldable t, Alternative t) => t a -> t a -> t a
append' = flip $ foldr ((<|>) . pure)

length' :: (Foldable t) => t a -> Integer
length' = foldr (const (+1)) 0
