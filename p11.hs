-- problem 11, return run-length encoding, but return individual
-- letter for non-repeats
import Data.List

data RunEncodedElement = Single Char | Multiple Int Char deriving Show

encode :: String -> [RunEncodedElement]
encode = map f . encodeTuples
  where f (1, x) = Single x
        f (l, x) = Multiple l x

encodeTuples :: Eq a => [a] -> [(Int, a)]
encodeTuples = map f . group
  where f = (\x -> (length x, head x))

encodeDirect :: String -> [RunEncodedElement]
encodeDirect = map f . group
  where f [x]      = Single x
        f xs@(x:_) = Multiple (length xs) x

main = do
  let lst = "aaaabccaadeeee"
  print $ encode lst
  print $ encodeDirect lst
