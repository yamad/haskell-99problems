{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All

import Data.List

-- problem 5, reverse a list

-- | naive list version
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- | tail recursive version
reverse'' :: [a] -> [a]
reverse'' = revAcc []
  where
    revAcc acc []     = acc
    revAcc acc (x:xs) = revAcc (x : acc) xs

-- | fold-based version
reverseFold :: [a] -> [a]
reverseFold = foldl' (flip (:)) []

prop_reverse'LikeBuiltin    xs = reverse' xs == reverse xs
prop_reverse''LikeBuiltin   xs = reverse'' xs == reverse xs
prop_reverseFoldLikeBuiltin xs = reverseFold xs == reverse xs

return []
main = $(quickCheckAll)
