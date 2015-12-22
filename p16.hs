-- problem 16, drop every @n@'th element of a list
import Criterion.Main

-- | first try, using splitAt. apparently splitAt is slower than take and drop
dropEverySplit :: Show a => [a] -> Int -> [a]
dropEverySplit [] _ = []
dropEverySplit xs 1 = xs
dropEverySplit xs n
  | n < 1     = []
  | otherwise = first ++ dropEverySplit rest' n
  where (first, rest) = splitAt (n-1) xs
        rest' = if null rest then rest else tail rest

-- | take/drop version. work in take and drop is almost entirely redundant, but
-- faster than @dropEverySplit@.
dropEveryTake :: [a] -> Int -> [a]
dropEveryTake [] _ = []
dropEveryTake xs n
  | n < 1     = []
  | otherwise = take (n-1) xs ++ dropEveryTake (drop n xs) n

-- | iterative helper version. by far the fastest.
dropEveryIterate :: [a] -> Int -> [a]
dropEveryIterate xs n = iter xs n
  where iter []     _ = []
        iter (y:ys) 1 = iter ys n
        iter (y:ys) k = y : iter ys (k-1)

-- | higher-order list processing version. about 2x slower than @dropEveryTake@
dropEveryList :: [a] -> Int -> [a]
dropEveryList xs n = map fst $ filter ((/=n) . snd) $ zip xs $ cycle [1..n]

main = do
  let expected = [1,3,5,7,9]
  print expected
  print $ dropEverySplit  [1..10] (-3)
  print $ dropEverySplit  [1..10] (-1)
  print $ dropEverySplit  [1]     2
  print $ ((dropEverySplit  []      10) :: [Bool])
  print $ dropEverySplit  [1..10] 2
  print $ dropEverySplit  [1..10] 3
  print $ dropEveryTake [1..10] 3
  print $ ((dropEveryTake [] 3) :: [Bool])
  print $ dropEveryIterate [1..10] 3
  print $ dropEveryList [1..10] 3
  print $ dropEveryIterate ['a'..'k'] 3
  defaultMain [
    bgroup "dropEvery" [ bench "dropEverySplit"   $ whnf (dropEverySplit  [1..10]) 3
                       , bench "dropEveryTake"  $ whnf (dropEveryTake [1..10]) 3
                       , bench "dropEveryIterate"  $ whnf (dropEveryIterate [1..10]) 3
                       , bench "dropEveryList"  $ whnf (dropEveryList [1..10]) 3
                       ]
    ]
