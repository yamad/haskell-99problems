{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- | find last but one element of a list
penultimate :: [a] -> a
penultimate []     = error "empty list"
penultimate [x,_]  = x
penultimate (_:xs) = penultimate xs

-- | composition-based version
penultimateCompose :: [a] -> a
penultimateCompose = second' . reverse'

-- | composition of built-ins
penultimateBuiltins :: [a] -> a
penultimateBuiltins = last . init

-- | get second element in list
second' :: [a] -> a
second' []  = error "empty list"
second' [x] = error "not enough items"
second' (x:y:xs) = y

-- | reverse a list
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = (reverse' xs) ++ [x]


prop_pen_correct x y xs =
  (x `notElem` xs) && (y `notElem` xs) ==>
  penultimate (xs++[y]++[x]) == y
prop_pen'_correct x y xs =
  (x `notElem` xs) && (y `notElem` xs) ==>
  penultimate' (xs++[y]++[x]) == y
prop_pen''_correct x y xs =
  (x `notElem` xs) && (y `notElem` xs) ==>
  penultimate'' (xs++[y]++[x]) == y

return []
main = $(quickCheckAll)
