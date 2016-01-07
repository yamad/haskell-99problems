-- problem 18, extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice xs i k = drop (i-1) $ take k xs

main = do
  print $ slice ['a'..'k'] 3 7
