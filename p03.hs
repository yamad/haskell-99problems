{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- | find the K'th element of a list
elementAt :: [a] -> Int -> a
-- elementAt _ n
--   | n <= 0   = error "zero or negative index"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)
elementAt [] _     = error "not enough items"

elementAt' :: [a] -> Int -> a
elementAt' xs n = xs !! (n-1)

prop_elem xs n = length xs > 0 && n > 0 ==> elementAt xs n == xs !! (n-1)

return []
main = $(quickCheckAll)
