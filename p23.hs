-- problem 23, extract a given number of randomly selected elements from a list
import System.Random
import Data.List

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = map (xs!!) <$> (take (min n (length xs - 1)) . nub <$> randomRs (0, length xs - 1) <$> newStdGen)

main = do
  print =<< rndSelect [10..20] 0
  print =<< rndSelect [10..20] 1
  print =<< rndSelect [10..20] 10
  print =<< rndSelect [10..20] 10
  print =<< rndSelect [10..20] 12
