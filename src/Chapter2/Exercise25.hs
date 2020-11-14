{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
module Chapter2.Exercise25 () where

import Chapter2.Exercise4 (cons, car, cdr)

-- | Give combinations of head and  that will pick 7 from each of the
-- following lists:
-- >>> l1 = (cons 1 (cons 3 (cons (cons 5 7) 9)))
-- >>> (cdr . car . cdr . cdr) l1
-- 7

-- >>> l2 = (cons (cons 7 1) 0)
-- >>> (car . car) l2
-- 7

-- >>> l3 = (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 7))))))
-- >>> (cdr . cdr . cdr . cdr . cdr . cdr) l3
-- 7
