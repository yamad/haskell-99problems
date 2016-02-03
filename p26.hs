-- problem 26, generate combinations of a list

combinations :: Int -> [a] -> [[a]]
combinations _ []     = []
combinations 1 xs     = map return xs
combinations k (x:xs)
  | k < 1     = []
  | otherwise = map (x:) (combinations (k-1) xs) ++ combinations k xs


main = do
  print $ combinations 3 ['a'..'e']
  print $ combinations 2 ['a'..'e']
  print $ combinations 1 ['a'..'e']
  print $ combinations (-100) ['a'..'e']
