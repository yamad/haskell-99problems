-- problem 24, draw N distinct random numbers from the set 1..M
import System.Random
import Data.List

rndSubset :: Int -> Int -> IO [Int]
rndSubset m n
  | n < 0     = return []
  | n >= m    = return shuffle [1..m]
  | otherwise = take n . nub <$> randomRs (1, m) <$> newStdGen

main = do
  print =<< rndSubset (-1) 3
  print =<< rndSubset 10 0
  print =<< rndSubset 10 1
  print =<< rndSubset 10 4
  print =<< rndSubset 10 4
  print =<< rndSubset 10 4
  print =<< rndSubset 4  4
  print =<< rndSubset 4  4
