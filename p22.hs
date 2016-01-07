-- problem 22, create a list containing all integers in a given range
range :: Int -> Int -> [Int]
range b e
  | b > e = []
  | otherwise = b : range (b+1) e

rangeIter :: Int -> Int -> [Int]
rangeIter = iter []
  where iter acc b e
          | b > e     = reverse acc
          | otherwise = iter (b  : acc) (b+1) e

main = do
  print $ range 1 1
  print $ range 0 (-2)
  print $ range 4 9
  print $ range (-4) 0
  print $ rangeIter 1 1
  print $ rangeIter 0 (-2)
  print $ rangeIter 4 9
  print $ rangeIter (-4) 0
