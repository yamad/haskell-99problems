-- problem 8, eliminate consecutive duplicates of list elements

compress :: (Eq a) => [a] -> [a]
compress []     = []
compress (x:xs) = x : comp' x xs
  where
    comp' y []    = []
    comp' y (z:zs)
      | y == z    = comp' y zs
      | otherwise = z : (comp' z zs)

compress' :: Eq a => [a] -> [a]
compress' []     = []
compress' (x:xs) = x : (compress' $ dropWhile (== x) xs)

compressFold :: (Eq a) => [a] -> [a]
compressFold = foldr skipRepeats []
  where
    skipRepeats x [] = [x]
    skipRepeats x acc
      | x == head acc  = acc
      | otherwise      = x : acc

-- from HaskellWiki, not sure how it works
compressFoldr xs = foldr f (const []) xs Nothing
  where
    f x r a@(Just q) | x == q = r a
    f x r _                   = x : r (Just x)

main = do
  print $ compress "aaaabccaadeeee"
  print $ compress [1,1,1,2,2,3,4,4,4,5,5,6,8,8]
  print $ compress' "aaaabccaadeeee"
  print $ compress' [1,1,1,2,2,3,4,4,4,5,5,6,8,8]
  print $ compressFold "aaaabccaadeeee"
  print $ compressFold [1,1,1,2,2,3,4,4,4,5,5,6,8,8]
  print $ compressFoldr "aaaabccaadeeee"
  print $ compressFoldr [1,1,1,2,2,3,4,4,4,5,5,6,8,8]
