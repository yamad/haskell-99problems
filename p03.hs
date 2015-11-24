{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- | find the K'th element of a list. first index is 1.
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)
elementAt [] _     = error "not enough items"

-- | record built-in equivalent
elementAtBuiltin :: [a] -> Int -> a
elementAtBuiltin xs n = xs !! (n-1)

prop_elem xs n =
  length xs > 0 && n > 0 && length xs >= n  ==>  elementAt xs n == xs !! (n-1)

return []
main = $(quickCheckAll)
