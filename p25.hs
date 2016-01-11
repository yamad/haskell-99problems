-- problem 25, generate a random permutation of the elements of a list
import System.Random
import Data.List
import Control.Applicative

-- | generate a random permutation
rndPermu :: [a] -> IO [a]
rndPermu xs = rndSelect xs (length xs)

-- | from problem 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = map (xs!!) <$> (take (min n (length xs - 1)) . nub <$> randomRs (0, length xs - 1) <$> newStdGen)

main = do
  print =<< rndPermu [0..10]
  print =<< rndPermu [0..10]
  print =<< rndPermu [0..10]
  print =<< rndPermu [0..10]
