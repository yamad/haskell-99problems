-- problem 21, insert an element at a given position into a list

insertAt :: a -> [a] -> Int -> [a]
insertAt e xs     1 = e : xs
insertAt e []     _ = []
insertAt e (x:xs) n = x : insertAt e xs (n-1)

insertAtIter :: a -> [a] -> Int -> [a]
insertAtIter a xs n
  | n < 1     = xs
  | otherwise = iter n a [] xs
  where
    iter 1 a acc xs     = reverse acc ++ (a : xs)
    iter _ a acc []     = reverse acc
    iter k a acc (x:xs) = iter (k-1) a (x : acc) xs

main = do
  print $ map (insertAt "alfa" ["a", "b", "c", "d"]) [0..6]
  print $ map (insertAtIter "alfa" ["a", "b", "c", "d"]) [0..6]
