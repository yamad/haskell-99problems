{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

last' :: [a] -> a
last' []     = error "No last in an empty list"
last' [x]    = x
last' (_:xs) = last' xs

last'' :: [a] -> a
last'' = head . reverse

prop_last' xs = length xs > 0 ==> last' xs == last xs
prop_last'' xs = length xs > 0 ==> last'' xs == last xs
prop_getappend x xs = x `notElem` xs ==> last'' (xs++[x]) == x

return []
main = $(quickCheckAll)
