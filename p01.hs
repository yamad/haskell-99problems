{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

import Data.List

-- | list-based version
last' :: [a] -> a
last' []     = error "No last in an empty list"
last' [x]    = x
last' (_:xs) = last' xs

-- | function composition version
lastCompose :: [a] -> a
lastCompose = head . reverse

-- | fold-based version
lastFold :: [a] -> a
lastFold = foldl1 (\_ x -> x)

-- | built-in function equivalent
lastBuiltin :: [a] -> a
lastBuiltin = last

prop_last' xs =
  length xs > 0 ==> last'       xs == last xs
prop_lastCompose xs =
  length xs > 0 ==> lastCompose xs == last xs
prop_lastFoldLikeBuiltin xs =
  length xs > 0 ==> lastBuiltin xs == last xs

return []
main = $(quickCheckAll)
