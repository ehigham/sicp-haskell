module Chapter2.Exercise39 (reverse', reverse'') where

import Chapter2.Exercise38 (foldLeft, foldRight)

-- | Complete the following definitions of `reverse` (exercise 2.18) in terms
-- of `foldRight` and `foldLeft` from evercise 2.38:

reverse' :: [a] -> [a]
reverse' = foldRight (\x xs -> xs ++ [x]) []

reverse'' :: [a] -> [a]
reverse'' = foldLeft (flip (:)) []
