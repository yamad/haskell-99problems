-- problem 15, replicate elements of a list a given number of times
import Criterion.Main
import Data.List

repli :: [a] -> Int -> [a]
repli []     _ = []
repli _      0 = []
repli (x:xs) n = replicate n x ++ repli xs n

repliList :: [a] -> Int -> [a]
repliList xs n = concat $ map (replicate n) xs

repliMonad :: [a] -> Int -> [a]
repliMonad xs n = xs >>= (replicate n)

-- | return list of @n@ repeats of value @c@
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n c = c : replicate' (n-1) c

main =
  print $ repli      [1..10] 2 >>=
  print $ repliList  [1..10] 2 >>=
  print $ repliMonad [1..10] 2 >>=
  defaultMain [
    bgroup "repli" [ bench "repli" $ whnf (repli [1..10]) 2
                   , bench "repliList" $ whnf (repliList [1..10]) 2
                   , bench "repliMonad" $ whnf (repliMonad [1..10]) 2
                   ]
    ]
