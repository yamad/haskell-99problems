-- problem 17, split a list into two parts at the given index
import Criterion.Main

split :: [a] -> Int -> ([a], [a])
split xs n
  | n < 1 = ([], xs)
  | otherwise = iter n [] xs
  where iter _ first []     = (reverse first, [])
        iter 0 first rest   = (reverse first, rest)
        iter k first (y:ys) = iter (k-1) (y:first) ys

main = do
  print $ split [1..10] 3
  print $ split ['a'..'k'] (-2)
  print $ split ['a'..'k'] 0
  print $ split ['a'..'k'] 5
  defaultMain [
    bgroup "split" [ bench "split" $ whnf (split [1..10]) 5
                   ]
    ]
