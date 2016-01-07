-- problem 20, remove the k'th element and return the residue

-- | iterative version.
removeAt :: [a] -> Int -> (Maybe a, [a])
removeAt xs n = iter n [] xs
  -- bail on improper index, not required for finite lists but saves time
  | n < 1         = (Nothing, xs)
  | otherwise     = iter n [] xs
  where
    -- collect the prefix list (reversed) in an accumulator
    iter _ acc []     = (Nothing, reverse acc)
    iter 1 acc (y:ys) = (Just y, reverse acc ++ ys)
    iter k acc (y:ys) = iter (k-1) (y:acc) ys

-- | recursive version. from Haskell Wiki
-- needed to trace this to understand it. the key is the two base
-- cases, which are the only way the residue @a@ is set--either
-- because we find the element we want to remove or the list runs
-- out. Once @a@ is set, then this value bubbles back out as the
-- recursive steps are evaluated, which involves just cons'ing the
-- list values before the residue onto the result list.
removeAtRecurse :: [a] -> Int -> (Maybe a, [a])
removeAtRecurse []     _ = (Nothing, [])
removeAtRecurse (x:xs) 1 = (Just x, xs)
removeAtRecurse (x:xs) n = (a, x:r)
  where (a, r) = removeAtRecurse xs (n-1)

main = do
  print $ map (removeAt [1..10]) [1..10]
  print $ removeAt [1..10] (-3)
  print $ removeAt [1..10] 11
  print $ ((removeAt [] 3) :: (Maybe Bool, [Bool]))
  print $ ((removeAt [] (-3)) :: (Maybe Bool, [Bool]))
  print $ map (removeAtRecurse [1..10]) [1..10]
  print $ removeAtRecurse [1..10] (-3)
  print $ removeAtRecurse [1..4] 5
  print $ removeAtRecurse [1..4] 3
  print $ ((removeAtRecurse [] (-3)) :: (Maybe Bool, [Bool]))
