{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All

import Data.List

-- | naive list-based version
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = (1+ length' xs)

-- | tail-recursive version
length'' :: [a] -> Int
length'' xs = len xs 0
  where
    len []     acc = acc
    len (x:xs) acc = len xs (1+ acc)

-- | fold version
lengthFold :: [a] -> Int
lengthFold = foldl' (\len x -> (+) 1 len) 0

-- | record built-in equivalent function
lengthBuiltin :: [a] -> Int
lengthBuiltin = length


prop_length'LikeBuiltin    xs = length'    xs == length xs
prop_length''LikeBuiltin   xs = length''   xs == length xs
prop_lengthFoldLikeBuiltin xs = lengthFold xs == length xs

return []
main = $(quickCheckAll)
