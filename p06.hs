{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- problem 6, palindrome predicate

-- | naive version
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- | faster version?
isPalindromeHalf :: Eq a => [a] -> Bool
isPalindromeHalf xs = (take half xs) == (take half (reverse xs))
  where half = length xs `div` 2

prop_isPalindromeEqualsReverseLists xs = isPalindromeHalf xs == isPalindrome xs

return []
main = $(quickCheckAll)
